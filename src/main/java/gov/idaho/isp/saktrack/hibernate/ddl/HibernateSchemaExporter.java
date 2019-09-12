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

package gov.idaho.isp.saktrack.hibernate.ddl;

import gov.idaho.isp.saktrack.hibernate.HibernateExporter;
import java.util.HashMap;
import java.util.Map;
import javax.sql.DataSource;
import org.hibernate.dialect.Dialect;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

/**
 * Utility class used to export database schema (DDL) as a SQL script
 */
public class HibernateSchemaExporter {
  public static void main(String[] args) {
    exportMariaDbSchema();
  }

  private static void exportMariaDbSchema() {
    DataSource ds = new DriverManagerDataSource("jdbc:mariadb://localhost/SexualAssaultKitTracking", "dev_user", "test");
    exportSchema(ds, org.hibernate.dialect.MariaDBDialect.class);
  }

  private static void exportH2Schema() {
    DataSource ds = new DriverManagerDataSource("jdbc:h2:file:C:/development/databases/sexaulAssaultKitTracking", "sa", "");
    exportSchema(ds, org.hibernate.dialect.H2Dialect.class);
  }

  private static void exportOracleSchema() {
    DataSource ds = new DriverManagerDataSource("jdbc:oracle:thin:@ie9:1521:XE", "sexual_assault_kit_user", "test");
    exportSchema(ds, org.hibernate.dialect.Oracle10gDialect.class);
  }

  private static void exportSchema(DataSource ds, Class<? extends Dialect> dialectClass) {
    Map<String,Object> settings = new HashMap<>();
    HibernateExporter.exportSchema(ds, dialectClass, settings, "gov.idaho.isp.saktrack");
  }
}
