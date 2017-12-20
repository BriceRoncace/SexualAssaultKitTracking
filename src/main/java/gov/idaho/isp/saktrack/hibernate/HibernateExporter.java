package gov.idaho.isp.saktrack.hibernate;

import java.util.Collections;
import java.util.EnumSet;
import java.util.Map;
import javax.sql.DataSource;
import org.hibernate.boot.Metadata;
import org.hibernate.boot.MetadataSources;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import static org.hibernate.cfg.AvailableSettings.DATASOURCE;
import static org.hibernate.cfg.AvailableSettings.DIALECT;
import org.hibernate.dialect.Dialect;
import org.hibernate.tool.hbm2ddl.SchemaExport;
import static org.hibernate.tool.schema.TargetType.DATABASE;
import static org.hibernate.tool.schema.TargetType.STDOUT;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.orm.hibernate5.LocalSessionFactoryBuilder;

public class HibernateExporter {

  public static void exportSchema(DataSource dataSource, Class<? extends Dialect> dialect, Map<String,Object> settings, String... packagesToScan) {
    StandardServiceRegistryBuilder registryBuilder = new StandardServiceRegistryBuilder()
      .applySettings(settings)
      .applySetting(DATASOURCE, dataSource)
      .applySetting(DIALECT, dialect); // dialect could be omitted
    MetadataSources metadataSources = new MetadataSources(registryBuilder.build());

    PathMatchingResourcePatternResolver resourceLoader = new PathMatchingResourcePatternResolver();
    new LocalSessionFactoryBuilder(null, resourceLoader, metadataSources).scanPackages(packagesToScan);

    Metadata metadata = metadataSources.buildMetadata();

    new SchemaExport().setFormat(true).create(EnumSet.of(STDOUT, DATABASE), metadata);
  }

  public static void exportSchema(DataSource dataSource, Class<? extends Dialect> dialect, String... packagesToScan) {
    exportSchema(dataSource, dialect, Collections.emptyMap(), packagesToScan);
  }
}