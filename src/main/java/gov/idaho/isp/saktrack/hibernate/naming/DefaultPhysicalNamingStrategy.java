package gov.idaho.isp.saktrack.hibernate.naming;


import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.boot.model.naming.PhysicalNamingStrategyStandardImpl;
import org.hibernate.cfg.ImprovedNamingStrategy;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;

/**
 * Internally uses Hibernate's ImprovedNamingStrategy to name tables and columns.
 *
 * See https://stackoverflow.com/a/40409625/225217
 */
public class DefaultPhysicalNamingStrategy extends PhysicalNamingStrategyStandardImpl {
  private static final ImprovedNamingStrategy STRATEGY_INSTANCE = new ImprovedNamingStrategy();

  @Override
  public Identifier toPhysicalTableName(Identifier name, JdbcEnvironment context) {
    return new Identifier(classToTableName(name.getText()), name.isQuoted());
  }

  @Override
  public Identifier toPhysicalColumnName(Identifier name, JdbcEnvironment context) {
    return new Identifier(STRATEGY_INSTANCE.classToTableName(name.getText()), name.isQuoted());
  }

  private String classToTableName(String className) {
    return STRATEGY_INSTANCE.classToTableName(className);
  }
}
