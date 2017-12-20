package gov.idaho.isp.saktrack.hibernate.naming;

import java.util.StringJoiner;
import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.boot.model.naming.PhysicalNamingStrategyStandardImpl;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;

/**
 * Uses the naming conventions used in SQL Server's example (AdventureWorks)
 * database.
 *
 * See https://stackoverflow.com/a/3593804/225217
 */
public class SqlServerPhysicalNamingStrategy extends PhysicalNamingStrategyStandardImpl {
  @Override
  public Identifier toPhysicalTableName(Identifier name, JdbcEnvironment context) {
    return new Identifier(toPascalCase(name.getText()), name.isQuoted());
  }

  @Override
  public Identifier toPhysicalColumnName(Identifier name, JdbcEnvironment context) {
    return new Identifier(toCamelCase(name.getText()), name.isQuoted());
  }

  private static String toCamelCase(String s) {
    return unCapitialize(toPascalCase(s));
  }

  private static String toPascalCase(String s) {
    if (s.contains(" ")) {
      return capitializeEachWord(s, " ");
    }

    if (s.contains("-")) {
      return capitializeEachWord(s, "-");
    }

    if (s.contains("_")) {
      return capitializeEachWord(s, "_");
    }

    return capitializeEachWord(s, null);
  }

  private static String capitializeEachWord(String s, String delimiter) {
    String[] words = delimiter != null ? s.split(delimiter) : new String[] {s};

    StringJoiner sj = new StringJoiner("");

    for (String word : words) {
      if (isUpperCase(word)) {
        word = word.toLowerCase();
      }
      sj.add(capitialize(word));
    }

    return sj.toString();
  }

  private static String capitialize(String s) {
    if (s.length() == 1) {
      s = s.substring(0, 1).toUpperCase();
    }
    else if (s.length() > 1) {
      s = s.substring(0, 1).toUpperCase() + s.substring(1);
    }
    return s;
  }

  private static String unCapitialize(String s) {
    if (s.length() == 1) {
      s = s.substring(0, 1).toLowerCase();
    }
    else if (s.length() > 1) {
      s = s.substring(0, 1).toLowerCase() + s.substring(1);
    }
    return s;
  }

  public static boolean isUpperCase(String s) {
    for (int i = 0; i < s.length(); i++) {
      if (!Character.isUpperCase(s.charAt(i))) {
        return false;
      }
    }
    return true;
  }
}