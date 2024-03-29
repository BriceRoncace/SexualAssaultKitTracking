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

package gov.idaho.isp.saktrack.hibernate;

import org.hibernate.boot.Metadata;
import org.hibernate.boot.MetadataSources;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.dialect.Dialect;
import org.hibernate.tool.hbm2ddl.SchemaExport;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.orm.hibernate5.LocalSessionFactoryBuilder;

import javax.sql.DataSource;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Map;

import static org.hibernate.cfg.AvailableSettings.DATASOURCE;
import static org.hibernate.cfg.AvailableSettings.DIALECT;
import static org.hibernate.tool.schema.TargetType.DATABASE;
import static org.hibernate.tool.schema.TargetType.STDOUT;

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