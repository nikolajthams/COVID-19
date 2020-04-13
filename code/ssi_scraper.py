from datetime import date
from urllib.error import HTTPError

import tabula

today = date.today().strftime("%d%m%Y")
ending = "-gy70"  # Changes daily


def get_timeseries(date):
    """
    Return daily testing data for the past 14 days prior to input date.
    """
    file = "https://files.ssi.dk/COVID19-overvaagningsrapport-" + date + ending

    top = 128.13
    left = 70.04
    width = 395.27
    height = 304.59



    try:
        tables = tabula.read_pdf(
            file, pages=9,
            multiple_tables=True,
            area=(top, left, top + height, left + width),
            stream=True)
    except HTTPError:
        return print("No data found for date " + date)
        return None

    # Depreceated ###
    # vals = ['Prøvedato' in tables[i].values.flatten()
    #         for i in range(len(tables))]
    # correct_table = [i for i, n in enumerate(vals) if n][0]
    ###################

    startval = 2

    clean_df1 = tables[0]
    a, b = clean_df1.shape
    clean_df1 = clean_df1.iloc[startval:a, :].rename(columns={
        "Unnamed: 0": "Date",
        "Laboratorie-bekræftede": "Lab confirmed cases",
        "Antal testede": "Tested",
        "Andel positive": "Rate"})

    # outfile = 'code/data/ssi.csv'
    # outfile = 'data/ssi.csv'
    # clean_df1.to_csv(outfile)
    return clean_df1  # print("Successfully exported file to path " + outfile)


def get_AllTables(date):
    """
    Return list of all tables in report.
    """
    file = "https://files.ssi.dk/COVID19-overvaagningsrapport-" + date + ending

    try:
        tables = tabula.read_pdf(
            file,
            pages='all',
            multiple_tables=True,
            stream=True)
    except HTTPError:
        print("No data found for date " + date)
        return None

    return tables


def get_AgeGroups(date):
    """
    Return cumulative testing data by age groups.
    """
    file = "https://files.ssi.dk/COVID19-overvaagningsrapport-" + date + ending
    # file = "https://files.ssi.dk/COVID19-overvaagningsrapport-29032020-f67s"

    top = 409.3
    left = 70.86
    width = 424.29
    height = 228.72

    try:
        tables = tabula.read_pdf(
            file, pages=11,
            multiple_tables=False,
            area=(top, left, top + height, left + width),
            stream=True)[0]
    except HTTPError:
        print("No data found for date " + date)
        return None

    a, b = tables.shape
    clean_df = tables.iloc[1:a, :]

    clean_df.iloc[:, 3] = clean_df.iloc[:, 3].str.replace(",", ".")

    for i in range(a - 1):  # Fix decimal inconsistencies.
        if clean_df.iloc[i, 2] < 100:
            clean_df.iloc[i, 2] = clean_df.iloc[i, 2] * 1000
        # if clean_df.iloc[i, 3] < 10:
        #     clean_df.iloc[i, 3] = clean_df.iloc[i, 2] * 1000

    if "Andel positive" in clean_df.columns.values:  # Fix naming inconsistencies.
        clean_df = clean_df.rename(
            columns={"Andel positive": "Procent positiv"})

    return clean_df


if __name__ == "__main__":
    # Get time series:
    _tmp1 = get_timeseries(today)
    _tmp1.to_csv("data/ssi.csv")

    # Get age data
    _tmp = get_AgeGroups(today)
    _tmp.to_csv("data/ssi_agegroups/data_" + today + ".csv")

    # Get all age data: #### Depreceated
    # avail_dates = ['12032020', '13032020', '16032020', '17032020',
    #                '18032020', '19032020', '20032020', '21032020', '22032020']
    # for date in avail_dates:
    #     datatable = get_AgeGroups(date)
    #     outfile = 'data/ssi_agegroups/data_' + date + '.csv'
    #     datatable.to_csv(outfile)
    #     print('Wrote to file ' + outfile)
