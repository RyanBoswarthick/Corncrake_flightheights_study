raw_data<-load_your_csvs("data/raw_data")

raw_data_cols<-select_gps_cols(raw_data)

raw_data_cols_formatted<-cleanyour_gpsdata_list(raw_data_cols)

full_dataset <- combine_gps_data(raw_data_cols_formatted)

full_dataset_clean<-cleanyour_gpsdata_please(full_dataset)

str(full_dataset_clean)
write.csv(full_dataset_clean, file = "outputs/01_fulldataset.csv", row.names = FALSE)
