download.file(
  "http://www.qogdata.pol.gu.se/data/qog_std_ts_jan18.dta",
  "data-raw/quality_of_government/qog_std_ts_jan18.dta"
  )
qog <- foreign::read.dta("data-raw/quality_of_government/qog_std_ts_jan18.dta")
