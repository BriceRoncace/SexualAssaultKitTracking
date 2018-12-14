# Sexual Assault Kit Tracking

## Version 2.0 (December 2018)

Version 2.0 of the application brings many improvements:

* Uses Spring Boot 2.1.1 (JDK 11 compatable; targets 1.8 by default)
* Authenticates all users from the database rather than using LDAP for admin users
* Removes all local dependencies (i.e. all necessary dependencies are avaialble publicly on maven central)
* Allows for quick-start demo in development mode by running the application on an embedded tomcat 9 server using a file based H2 database (see the video below to see this in action).

### Quick Start

[![Watch the video](https://img.youtube.com/vi/27eglNmnEpU/maxresdefault.jpg)](https://youtu.be/27eglNmnEpU)

The video above shows how to quickly get up and running with the application.  It's as simple as:

1. Download or clone source code

2. Maven build 
```
SexualAssaultKitTracking-2.0> mvn clean install
...
```

3. CD to target directory and execute the .war file
```
SexualAssaultKitTracking-2.0\target>java -jar SexualAssaultKitTracking-2.0.war

  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/
 :: Spring Boot ::        (v2.1.1.RELEASE)

2018-12-11 11:50:06.554  INFO 15344 --- [           main] gov.idaho.isp.saktrack.Application       : Starting Application v2.0 on 9IT-C69705 with PID 15344 (C:\development\github\SexualAssaultKitTracking-2.0\target\SexualAssaultKitTracking-2.0.war started by broncace in c:\development\github\SexualAssaultKitTracking-2.0\target)
2018-12-11 11:50:06.556 DEBUG 15344 --- [           main] gov.idaho.isp.saktrack.Application       : Running with Spring Boot v2.1.1.RELEASE, Spring v5.1.3.RELEASE
2018-12-11 11:50:06.557  INFO 15344 --- [           main] gov.idaho.isp.saktrack.Application       : The following profiles are active: dev
2018-12-11 11:50:07.158  INFO 15344 --- [           main] .s.d.r.c.RepositoryConfigurationDelegate : Bootstrapping Spring Data repositories in DEFAULT mode.
2018-12-11 11:50:07.251  INFO 15344 --- [           main] .s.d.r.c.RepositoryConfigurationDelegate : Finished Spring Data repository scanning in 88ms. Found 9 repository interfaces.
2018-12-11 11:50:07.582  INFO 15344 --- [           main] trationDelegate$BeanPostProcessorChecker : Bean 'org.springframework.transaction.annotation.ProxyTransactionManagementConfiguration' of type [org.springframework.transaction.annotation.ProxyTransactionManagementConfiguration$$EnhancerBySpringCGLIB$$b07f768a] is not eligible for getting processed by all BeanPostProcessors (for example: not eligible for auto-proxying)
2018-12-11 11:50:07.730  INFO 15344 --- [           main] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Starting...
2018-12-11 11:50:07.877  INFO 15344 --- [           main] com.zaxxer.hikari.HikariDataSource       : HikariPool-1 - Start completed.
2018-12-11 11:50:07.926  INFO 15344 --- [           main] o.hibernate.jpa.internal.util.LogHelper  : HHH000204: Processing PersistenceUnitInfo [
        name: default
        ...]
2018-12-11 11:50:07.989  INFO 15344 --- [           main] org.hibernate.Version                    : HHH000412: Hibernate Core {5.3.7.Final}
2018-12-11 11:50:07.990  INFO 15344 --- [           main] org.hibernate.cfg.Environment            : HHH000206: hibernate.properties not found
2018-12-11 11:50:08.094  INFO 15344 --- [           main] o.hibernate.annotations.common.Version   : HCANN000001: Hibernate Commons Annotations {5.0.4.Final}
2018-12-11 11:50:08.416  INFO 15344 --- [           main] org.hibernate.dialect.Dialect            : HHH000400: Using dialect: org.hibernate.dialect.H2Dialect
2018-12-11 11:50:09.307  INFO 15344 --- [           main] o.h.h.i.QueryTranslatorFactoryInitiator  : HHH000397: Using ASTQueryTranslatorFactory
2018-12-11 11:50:09.388  INFO 15344 --- [           main] j.LocalContainerEntityManagerFactoryBean : Initialized JPA EntityManagerFactory for persistence unit 'default'
2018-12-11 11:50:09.959  INFO 15344 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat initialized with port(s): 8080 (http)
2018-12-11 11:50:09.983  INFO 15344 --- [           main] o.apache.catalina.core.StandardService   : Starting service [Tomcat]
2018-12-11 11:50:09.983  INFO 15344 --- [           main] org.apache.catalina.core.StandardEngine  : Starting Servlet Engine: Apache Tomcat/9.0.13
2018-12-11 11:50:09.994  INFO 15344 --- [           main] o.a.catalina.core.AprLifecycleListener   : The APR based Apache Tomcat Native library which allows optimal performance in production environments was not found on the java.library.path: [C:\Program Files\Java\jdk-11.0.1+13\bin;C:\Windows\Sun\Java\bin;C:\Windows\system32;C:\Windows;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\Windows\System32\OpenSSH\;C:\Program Files\TortoiseSVN\bin;C:\Program Files\SlikSvn\bin;C:\Program Files (x86)\Microsoft SQL Server\Client SDK\ODBC\130\Tools\Binn\;C:\Program Files (x86)\Microsoft SQL Server\140\Tools\Binn\;C:\Program Files (x86)\Microsoft SQL Server\140\DTS\Binn\;C:\Program Files (x86)\Microsoft SQL Server\140\Tools\Binn\ManagementStudio\;C:\Program Files\PuTTY\;C:\development\maven\apache-maven-3.6.0\bin;C:\Program Files\Java\jdk-11.0.1+13\bin;C:\Oracle\instantclient_11_2;C:\Program Files\Curl\curl-7.62.0-win64-mingw\bin;C:\Program Files\Git\cmd;C:\Users\broncace\AppData\Local\Microsoft\WindowsApps;;.]
WARNING: An illegal reflective access operation has occurred
WARNING: Illegal reflective access by org.springframework.boot.loader.jar.Handler (file:/C:/development/github/SexualAssaultKitTracking-2.0/target/SexualAssaultKitTracking-2.0.war) to method java.net.URLStreamHandler.openConnection(java.net.URL)
WARNING: Please consider reporting this to the maintainers of org.springframework.boot.loader.jar.Handler
WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations
WARNING: All illegal access operations will be denied in a future release
2018-12-11 11:50:11.799  INFO 15344 --- [           main] org.apache.jasper.servlet.TldScanner     : At least one JAR was scanned for TLDs yet contained no TLDs. Enable debug logging for this logger for a complete list of JARs that were scanned but no TLDs were found in them. Skipping unneeded JARs during scanning can improve startup time and JSP compilation time.
2018-12-11 11:50:12.116  INFO 15344 --- [           main] .a.c.c.C.[.[.[/SexualAssaultKitTracking] : Initializing Spring embedded WebApplicationContext
2018-12-11 11:50:12.116  INFO 15344 --- [           main] o.s.web.context.ContextLoader            : Root WebApplicationContext: initialization completed in 5526 ms
2018-12-11 11:50:12.802  INFO 15344 --- [           main] o.s.s.web.DefaultSecurityFilterChain     : Creating filter chain: any request, [org.springframework.security.web.context.request.async.WebAsyncManagerIntegrationFilter@4de91056, org.springframework.security.web.context.SecurityContextPersistenceFilter@2d9de284, org.springframework.security.web.header.HeaderWriterFilter@660296d5, org.springframework.security.web.csrf.CsrfFilter@432c0f1, org.springframework.security.web.authentication.logout.LogoutFilter@3d6778d5, org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter@43755e2f, org.springframework.security.web.savedrequest.RequestCacheAwareFilter@7540160e, org.springframework.security.web.servletapi.SecurityContextHolderAwareRequestFilter@5ca22e19, org.springframework.security.web.authentication.AnonymousAuthenticationFilter@2404abe2, org.springframework.security.web.session.SessionManagementFilter@75d97e18, org.springframework.security.web.access.ExceptionTranslationFilter@573a078, org.springframework.security.web.access.intercept.FilterSecurityInterceptor@3b30eec5]
2018-12-11 11:50:12.937  INFO 15344 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2018-12-11 11:50:12.978  WARN 15344 --- [           main] aWebConfiguration$JpaWebMvcConfiguration : spring.jpa.open-in-view is enabled by default. Therefore, database queries may be performed during view rendering. Explicitly configure spring.jpa.open-in-view to disable this warning
2018-12-11 11:50:13.107  INFO 15344 --- [           main] o.s.s.c.ThreadPoolTaskScheduler          : Initializing ExecutorService 'taskScheduler'
2018-12-11 11:50:13.173  INFO 15344 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8080 (http) with context path '/SexualAssaultKitTracking'
2018-12-11 11:50:13.175  INFO 15344 --- [           main] gov.idaho.isp.saktrack.Application       : Started Application in 6.999 seconds (JVM running for 7.469)
2018-12-11 11:50:31.492  INFO 15344 --- [nio-8080-exec-1] .a.c.c.C.[.[.[/SexualAssaultKitTracking] : Initializing Spring DispatcherServlet 'dispatcherServlet'
2018-12-11 11:50:31.494  INFO 15344 --- [nio-8080-exec-1] o.s.web.servlet.DispatcherServlet        : Initializing Servlet 'dispatcherServlet'
2018-12-11 11:50:31.501  INFO 15344 --- [nio-8080-exec-1] o.s.web.servlet.DispatcherServlet        : Completed initialization in 7 ms
```

4. Browse to http://localhost:8080/SexualAssaultKitTracking


### Configuration

The application is Maven based.  By default the **dev** profile is active.  When you run Maven goals without a profile explicitly defined, **dev** will be used:

```
> mvn clean install
```

The application contains a **prod** profile but this profile must be explicitly specified like so:

```
> mvn -Pprod clean install
```

When the prod profile is used, the **/src/main/resources/application-prod.properties** will be referenced and will override any property found in the base properties file, **/src/main/resources/application.properties**.  In this way, for example, the application can be configured to, for example, disable the **mail.test.mode** property in production.

Here's a snippet of the applicatn-prod.properties file:

```
mail.test.mode = false
```

### Database Configuration

Using the default dev Maven profile, only the **/src/main/resources/application.properties** Spring Boot configuration file will be used.  This file specifies an H2 database:

```
# H2
spring.h2.console.enabled = true
spring.datasource.url = jdbc:h2:file:~/SexualAssaultKitTracking;DB_CLOSE_ON_EXIT=FALSE
spring.datasource.driver-class-name = org.h2.Driver
spring.datasource.username = sa
spring.datasource.password = 
spring.jpa.hibernate.ddl-auto = update
spring.jpa.show-sql = false
```

The above configuration will attempt to create a new SexualAssaultKitTracking H2 database in the home directory of the user running the application.  The database is file based and will therefore persist data between application restarts.

This setup requires that the H2 dependencies are available which is the case based on the Maven pom.xml:

```
<dependency>
  <groupId>com.h2database</groupId>
  <artifactId>h2</artifactId>
  <scope>runtime</scope>
</dependency>
```

To change the database used, for example to MariaDB, change the Maven dependencies:

```
<dependency>
  <groupId>org.mariadb.jdbc</groupId>
  <artifactId>mariadb-java-client</artifactId>
  <scope>runtime</scope>
</dependency>

<!-- H2 dependency no longer needed; can be commented out or removed completely
<dependency>
  <groupId>com.h2database</groupId>
  <artifactId>h2</artifactId>
  <scope>runtime</scope>
</dependency>
-->
```

And now the application.properties file can be updated to contain the connection information for MariaDB:

```
# MariaDB
spring.jpa.database-platform=org.hibernate.dialect.MariaDB102Dialect
spring.datasource.url=jdbc:mariadb://localhost:3306/SexualAssaultKitTracking
spring.datasource.driver-class-name=org.mariadb.jdbc.Driver
spring.datasource.username = dev_user
spring.datasource.password = test
spring.jpa.generate-ddl = true
spring.jpa.show-sql = false
```

### View Customizations

There is only one view that explicitly mentions "Idaho" that will need to be modified to accomodate a different state.  That view is: **src/main/webapp/WEB-INF/views/public/dashboard.jsp**.  This page contains an image of the Idaho seal as well as verbiage concerning the legislation unique to the State of Idaho authorizing the tracking of sexual assault kits.  Modify this page as needed.


## Version 1.0 (August 2018)

Demonstration of application setup and configuration on Windows + Tomcat:
* Using Oracle Database: https://youtu.be/5Q8FddWxNQw
* Using SQL Server Database: https://youtu.be/DJpnjM7zYds

