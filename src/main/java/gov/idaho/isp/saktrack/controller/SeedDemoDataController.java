/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.datasource.init.DatabasePopulator;
import org.springframework.jdbc.datasource.init.DatabasePopulatorUtils;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import javax.sql.DataSource;
import java.util.Optional;

@Controller
@Profile("dev")
public class SeedDemoDataController {

  public enum Database {
    H2("org.h2.Driver", "sql/data-h2.sql"),
    MSSQL("com.microsoft.sqlserver.jdbc.SQLServerDriver", "sql/data-mssql.sql");

    private final String driverClassName;
    private final String demoDataSql;

    Database(String driverClassName, String demoDataSql) {
      this.driverClassName = driverClassName;
      this.demoDataSql = demoDataSql;
    }

    public String getDriverClassName() {
      return driverClassName;
    }

    public String getDemoDataSql() {
      return demoDataSql;
    }

    public static Optional<Database> valueOfDriver(String driver) {
      if (driver.equalsIgnoreCase(H2.getDriverClassName())) {
        return Optional.of(H2);
      }
      else if (driver.equalsIgnoreCase(MSSQL.getDriverClassName())) {
        return Optional.of(MSSQL);
      }
      else {
        return Optional.empty();
      }
    }
  }

  private final Database database;
  private final DataSource dataSource;

  public SeedDemoDataController(@Value("${spring.datasource.driver-class-name}") String dbDriver, DataSource dataSource) {
    this.database = Database.valueOfDriver(dbDriver).orElse(null);
    this.dataSource = dataSource;
  }
  
  @PostMapping("/seedDemoData")
  public String seedDemoData(RedirectAttributes ra) {
    if (database != null) {
      try {
        Resource demoData = new ClassPathResource(database.getDemoDataSql());
        DatabasePopulator databasePopulator = new ResourceDatabasePopulator(demoData);
        DatabasePopulatorUtils.execute(databasePopulator, dataSource);
        ra.addFlashAttribute("messages", "Demo data imported into the" + database.name() + " database.");
      }
      catch (RuntimeException ex) {
        System.out.println(ex);
        ra.addFlashAttribute("errors", "Demo data failed to import (or has already been imported).");
      }
    }
    else {
      ra.addFlashAttribute("errors", "Demo data only intended for an H2 or SQL databases.");
    }
    return "redirect:/";
  }
}