package gov.idaho.isp.saktrack.service;

import java.util.Map;

public interface TemplateEvaluator {
  @FunctionalInterface
  interface Template {
    String getClasspathResourceName();
  }

  String evaluate(String template, Map<String,String> parameters);
  String evaluate(Template template, Map<String,String> parameters);
}