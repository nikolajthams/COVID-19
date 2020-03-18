from datetime import date

import tabula

today = date.today().strftime("%d%m%Y")

file = "https://www.ssi.dk/-/media/arkiv/dk/aktuelt/sygdomsudbrud/covid19-rapport/" + today + "/covid19-overvaagningsrapport-" + today + ".pdf"
tables = tabula.read_pdf(file, pages=2, multiple_tables=True, stream=True)

clean_df1 = tables[0]
a, b = clean_df1.shape
clean_df1 = clean_df1.iloc[3:(a - 1), :].rename(columns={
    "Unnamed: 0": "Date", "Laboratorie-": "Cases", "Unnamed: 2": "Tested", "Unnamed: 3": "Rate"})

outfile = 'code/data/ssi.csv'
clean_df1.to_csv(outfile)
