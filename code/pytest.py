from datetime import date
from urllib.error import HTTPError

import tabula

today = date.today().strftime("%d%m%Y")


def get_timeseries(date):
    file = "https://files.ssi.dk/COVID19-overvaagningsrapport-" + date

    try:
        tables = tabula.read_pdf(file, pages=2, multiple_tables=True, stream=True)
    except HTTPError:
        return print("No data found for date " + date)

    clean_df1 = tables[0]
    a, b = clean_df1.shape
    clean_df1 = clean_df1.iloc[3:(a - 1), :].rename(columns={
        "Unnamed: 0": "Date",
        "Laboratorie-": "Lab confirmed cases",
        "Unnamed: 1": "Tested",
        "Unnamed: 2": "Rate"})

    outfile = 'data/ssi.csv'
    clean_df1.to_csv(outfile)
    return print("Successfully exported file to path " + outfile)


def get_AllTables(date):
    file = "https://files.ssi.dk/COVID19-overvaagningsrapport-" + date

    try:
        tables = tabula.read_pdf(file, pages='all', multiple_tables=True, stream=True)
    except HTTPError:
        print("No data found for date " + date)
        return None

    return tables


def get_AgeGroups(date):
    tables = get_AllTables(date)

    if tables is None:
        print("No data found for date " + date)
        return None
    else:
        vals = ['Aldersgrupper' in tables[i].columns for i in range(len(tables))]
        agegroups = [i for i, n in enumerate(vals) if n][0]

    return tables[agegroups].drop([0, tables[agegroups].shape[0]-1])


###### Get age data:
avail_dates = ['12032020', '13032020', '16032020','17032020', '18032020', '19032020']
for date in avail_dates:
    datatable = get_AgeGroups(date)
    outfile = 'data/ssi_agegroups/data_' + date + '.csv'
    datatable.to_csv(outfile)
    print('Wrote to file ' + outfile)


###### Testing data
tables = tabula.read_pdf(file, pages=2, multiple_tables=True, stream=True)

clean_df1 = tables[0]
a, b = clean_df1.shape
clean_df1 = clean_df1.iloc[3:(a - 1), :].rename(columns={
    "Unnamed: 0": "Date", "Laboratorie-": "Lab confirmed cases", "Unnamed: 1": "Tested", "Unnamed: 2": "Rate"})

outfile = 'code/data/ssi.csv'
clean_df1.to_csv(outfile)

clean_df2 = tables[1]
a, b = clean_df2.shape
clean_df2 = clean_df2.iloc[4:(a-1), :].rename(columns={"Unnamed: 0": "Region", "Unnamed: 1": "Intensive care"})

outfile2 = 'code/data/ssi2.csv'
clean_df2.to_csv(outfile2)


###### Map data
tables = tabula.read_pdf(file, pages=3, multiple_tables=False, stream=True)
clean_df3 = tables[0].rename(columns={"Laboratoriebekræftede": "Lab confirmed cases"})
a, b = clean_df3.shape
clean_df3 = clean_df3.iloc[range(1, a-1), :]

clean_df3.to_csv('code/data/ssi_map.csv')
