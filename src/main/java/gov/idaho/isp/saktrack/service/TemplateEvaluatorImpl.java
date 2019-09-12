package gov.idaho.isp.saktrack.service;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.text.StringSubstitutor;
import org.apache.commons.text.lookup.StringLookup;
import org.springframework.stereotype.Service;

@Service
public class TemplateEvaluatorImpl implements TemplateEvaluator {
  private static final String DEFAULT_VAR_PREFIX = "${";
  private static final String DEFAULT_VAR_SUFFIX = "}";
  private static final String INCLUDE_KEYWORD = "include:";

  private String varPrefix = DEFAULT_VAR_PREFIX;
  private String varSuffix = DEFAULT_VAR_SUFFIX;
  private char escapeChar = StringSubstitutor.DEFAULT_ESCAPE;

  private String includePrefix = varPrefix + INCLUDE_KEYWORD;
  private String includeSuffix = varSuffix;

  @Override
  public String evaluate(String template, Map<String,String> parameters) {
    StringSubstitutor sub = new StringSubstitutor(new DefaultingToEmptyStringLookup(parameters), varPrefix, varSuffix, escapeChar);
    template = resolveIncludes(template);
    return sub.replace(template);
  }

  @Override
  public String evaluate(Template template, Map<String, String> values) {
    return evaluate(load(template), values);
  }

  private boolean hasIncludes(String template) {
    return template != null && template.contains(includePrefix);
  }

  private String resolveIncludes(String template) {
    int i = 0;
    while (hasIncludes(template)) {
      if (i++ > 100) {
        throw new RuntimeException("More than 100 include resolutions made. Probable cause: one include is including its caller (or itself) resulting in an endless loop.");
      }
      int includeStartIndex = template.indexOf(includePrefix);
      int includeEndIndex = template.indexOf(includeSuffix, includeStartIndex) + includeSuffix.length();
      String include = template.substring(includeStartIndex, includeEndIndex);
      template = template.replace(include, load(toTemplate(include)));
    }

    return template;
  }

  private Template toTemplate(String include) {
    int pathStartIndex = include.indexOf(includePrefix) + includePrefix.length();
    int pathEndIndex = include.indexOf(includeSuffix, pathStartIndex);
    return () -> include.substring(pathStartIndex, pathEndIndex);
  }

  private String load(Template template) {
    InputStream inputStream = getClass().getResourceAsStream(template.getClasspathResourceName());
    return new BufferedReader(new InputStreamReader(inputStream)).lines().collect(Collectors.joining("\n"));
  }

  public void setVarPrefix(String varPrefix) {
    this.varPrefix = varPrefix;
    this.includePrefix = varPrefix + INCLUDE_KEYWORD;
  }

  public String getVarPrefix() {
    return varPrefix;
  }

  public void setVarSuffix(String varSuffix) {
    this.varSuffix = varSuffix;
    this.includeSuffix = varSuffix;
  }

  public String getVarSuffix() {
    return varSuffix;
  }

  public String getIncludePrefix() {
    return includePrefix;
  }

  public String getIncludeSuffix() {
    return includeSuffix;
  }

  public void setEscapeChar(char escapeChar) {
    this.escapeChar = escapeChar;
  }

  public char getEscapeChar() {
    return escapeChar;
  }

  private static class DefaultingToEmptyStringLookup implements StringLookup {
    private final Map<String,String> values;

    public DefaultingToEmptyStringLookup(Map<String, String> values) {
      this.values = values;
    }

    @Override
    public String lookup(String key) {
      String value = values != null ? values.get(key) : null;
      return value != null ? value : "";
    }
  }
}