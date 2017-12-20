package gov.idaho.isp.saktrack.hibernate.ddl;

import gov.idaho.isp.saktrack.hibernate.HibernateExporter;
import gov.idaho.isp.saktrack.hibernate.naming.DefaultPhysicalNamingStrategy;
import java.util.HashMap;
import java.util.Map;
import javax.sql.DataSource;
import org.hibernate.cfg.AvailableSettings;
import org.hibernate.dialect.Dialect;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

/**
 * Utility class used to export database schema (DDL) as a SQL script
 */
public class HibernateSchemaExporter {
  public static void main(String[] args) {
    //exportMariaDbSchema();
  }

  private static void exportMariaDbSchema() {
    DataSource ds = new DriverManagerDataSource("jdbc:mariadb://poopsmith/sak", "sak_user", "test");
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
    settings.put(AvailableSettings.PHYSICAL_NAMING_STRATEGY, DefaultPhysicalNamingStrategy.class);
    HibernateExporter.exportSchema(ds, dialectClass, settings, "gov.idaho.isp.saktrack");
  }
}
