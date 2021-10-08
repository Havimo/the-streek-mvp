CREATE TABLE fact_streek_summary
  ( 
    user_id INT REFERENCES dim_user
    , streek_type VARCHAR
    , current_streek_value INT
    , max_streek_value INT
  );