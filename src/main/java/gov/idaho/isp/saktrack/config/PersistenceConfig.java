package gov.idaho.isp.saktrack.config;

import gov.idaho.isp.saktrack.hibernate.SpringAwareHibernateInterceptor;
import javax.sql.DataSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jndi.JndiObjectFactoryBean;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.Database;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(transactionManagerRef = "getTransactionManager", entityManagerFactoryRef = "getEntityManagerFactory", basePackages = "gov.idaho.isp.saktrack")
public class PersistenceConfig {
  @Value("${db.type}")
  private Database database;

  @Value("${db.hibernate.dialect}")
  private String databaseDialect;

  @Value("${db.hibernate.show.sql}")
  private boolean showSql;

  @Value("${db.hibernate.generate.ddl}")
  private boolean generateDdl;

  @Autowired
  private SpringAwareHibernateInterceptor springAwareHibernateInterceptor;

  @Bean
  public LocalContainerEntityManagerFactoryBean getEntityManagerFactory(DataSource dataSource) {
    LocalContainerEntityManagerFactoryBean lef = new LocalContainerEntityManagerFactoryBean();
    lef.setDataSource(dataSource);
    lef.setJpaVendorAdapter(getHibernateJpaVendorAdapter());
    lef.setPackagesToScan("gov.idaho.isp.saktrack");
    lef.getJpaPropertyMap().put("hibernate.session_factory.interceptor", springAwareHibernateInterceptor);
    //lef.getJpaPropertyMap().put("hibernate.id.new_generator_mappings", false); // added to allow SQL Server to use identiy for ids when using GenerationType.Auto; see http://stackoverflow.com/a/36182288/225217
    lef.getJpaPropertyMap().put("hibernate.physical_naming_strategy", "gov.idaho.isp.saktrack.hibernate.naming.DefaultPhysicalNamingStrategy");
    return lef;
  }

  private HibernateJpaVendorAdapter getHibernateJpaVendorAdapter() {
    HibernateJpaVendorAdapter hibernateJpaVendorAdapter = new HibernateJpaVendorAdapter();
    hibernateJpaVendorAdapter.setDatabase(database);
    hibernateJpaVendorAdapter.setDatabasePlatform(databaseDialect);
    hibernateJpaVendorAdapter.setShowSql(showSql);
    hibernateJpaVendorAdapter.setGenerateDdl(generateDdl);
    return hibernateJpaVendorAdapter;
  }

  @Bean
  public JndiObjectFactoryBean getDataSource() {
    JndiObjectFactoryBean jndiFactoryBean = new JndiObjectFactoryBean();
    jndiFactoryBean.setJndiName("java:comp/env/jdbc/saktrackDS");
    return jndiFactoryBean;
  }

  @Bean
  public JpaTransactionManager getTransactionManager(DataSource dataSource) {
    JpaTransactionManager jpaTransactionManager = new JpaTransactionManager();
    jpaTransactionManager.setEntityManagerFactory(getEntityManagerFactory(dataSource).getObject());
    jpaTransactionManager.setDataSource(dataSource);
    return jpaTransactionManager;
  }
}