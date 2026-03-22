



library(rsparkling)
library(sparklyr)
library(dplyr)
library(h2o)

library(sparklyr)
conf <- spark_config() 
conf$`sparklyr.shell.conf` <- c( 
  "spark.executor.extraJavaOptions=-Duser.timezone='UTC'", 
  "spark.driver.extraJavaOptions=-Duser.timezone='UTC'", 
  "spark.sql.session.timeZone='UTC'" 
) 
sc <- spark_connect( 
  master = "spark://HOST:PORT", config = conf 
) 
connection_is_open(sc) 
spark_disconnect(sc) 


sc <- spark_connect(master = "local")

mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

mtcars_h2o <- as_h2o_frame(sc, mtcars_tbl, strict_version_check = FALSE)

mtcars_glm <- h2o.glm(x = c("wt", "cyl"),
                      y = "mpg",
                      training_frame = mtcars_h2o,
                      lambda_search = TRUE)

mtcars_glm

spark_disconnect(sc)


livy_install()

