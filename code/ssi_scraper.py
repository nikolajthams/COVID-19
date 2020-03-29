from datetime import date
from urllib.error import HTTPError

import tabula

today = date.today().strftime("%d%m%Y")
ending = "-f67s"  # Changes daily


def get_timeseries(date):
    """
    Return daily testing data for the past 14 days prior to input date.
    """
    file = "https://files.ssi.dk/COVID19-overvaagningsrapport-" + date + ending

    try:
        tables = tabula.read_pdf(
            file, pages=8,
            multiple_tables=True,
            area=(127.34, 70.94, 324.95 + 127.34, 396.55 + 70.94),
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

    outfile = 'code/data/ssi.csv'
    clean_df1.to_csv(outfile)
    return print("Successfully exported file to path " + outfile)


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

    top = 432.82
    left = 71.6
    width = 423.18
    height = 227.72

    try:
        tables = tabula.read_pdf(
            file, pages=10,
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
        if clean_df.iloc[i, 2] < 10:
            clean_df.iloc[i, 2] = clean_df.iloc[i, 2] * 1000

    if "Andel positive" in clean_df.columns.values:  # Fix naming inconsistencies.
        clean_df = clean_df.rename(
            columns={"Andel positive": "Procent positiv"})

    return clean_df


if __name__ == "__main__":
    # Get time series:
    get_timeseries(today)

    # Get age data
    _tmp = get_AgeGroups(today)
    _tmp.to_csv("code/data/ssi_agegroups/data_" + today + ".csv")
    "Andel positive" in _tmp.columns.values

    # Get all age data: #### Depreceated
    # avail_dates = ['12032020', '13032020', '16032020', '17032020',
    #                '18032020', '19032020', '20032020', '21032020', '22032020']
    # for date in avail_dates:
    #     datatable = get_AgeGroups(date)
    #     outfile = 'data/ssi_agegroups/data_' + date + '.csv'
    #     datatable.to_csv(outfile)
    #     print('Wrote to file ' + outfile)
