library(reticulate)
use_condaenv("gee-demo", conda = "auto")
py_config()
py_available()


ee = import("ee")
ee$Initialize()
np = import("numpy")
pd = import("pandas")

