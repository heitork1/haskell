regressaoPI 0 0 0 = 0 
regressaoPI n d p = 
  if d <= n
    then 
      p*(4/d) + regressaoPI n (d + 2) ((-1)*p)
    else 
      regressaoPI 0 0 0

seriePI n = regressaoPI n 1 1