Sys.setenv(JAVA_HOME='/path/to/java_home')
options(java.parameters="-Xmx2g")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

# Create connection driver and open connection
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="~/ojdbc6.jar")
jdbcConnection <- 
  dbConnect(jdbcDriver, "jdbc:oracle:thin:@//192.00.00.0:8080/db", "user", "haslo")

# Query on the Oracle instance name.
instanceName1 <- dbGetQuery(jdbcConnection, "select * from unt")

for (aa in 2:length(instanceName1[,1])){
  q1<-paste ("select * from ", instanceName1[aa,1],".userstt 
             where  is null" , sep = " ", collapse = NULL)
  instanceName2 <- dbGetQuery(jdbcConnection, q1)
  if(length(instanceName2[,1])>0){
    csv_export1<-cbind(instanceName1[aa,1],instanceName1[aa,2], instanceName2)
    csv_export<-rbind(csv_export,csv_export1)}
}

write.csv(csv_export,file = "user.csv")
# Close connection
dbDisconnect(jdbcConnection)


Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
library(rJava)
library(RJDBC)

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="~/ojdbc6.jar")
driver <- JDBC("com.amazon.redshift.jdbc41.Driver", "RedshiftJDBC41-1.1.9.1009.jar", identifier.quote="`")

jdbcConnection <- 
  dbConnect(jdbcDriver, "jdbc:redshift://17439/rhall", "rheead", "A1dbC")

jdbcConnection <- 
  dbConnect(jdbcDriver, "jdbc:redshift://17.3:39/thupp",  "thead", "L16")

jdbcConnection <- 
  dbConnect(jdbcDriver, "jdbc:redshift://173:39/rad",  "rad", "Ex")


