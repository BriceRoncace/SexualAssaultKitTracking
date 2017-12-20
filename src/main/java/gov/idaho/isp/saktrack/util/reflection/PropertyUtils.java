package gov.idaho.isp.saktrack.util.reflection;

import java.lang.reflect.Field;
import java.util.Optional;

public class PropertyUtils {
  public static Object getProperty(Object bean, String name) {
    return getPropertyValue(bean, name);
  }

  public static <T> T getProperty(Object bean, String name, Class<T> clazz) {
    return (T) getPropertyValue(bean, name);
  }

  public static Object getPropertyOrNull(Object bean, String name) {
    try {
      return getPropertyValue(bean, name);
    }
    catch (Exception ex) {
      return null;
    }
  }

  public static <T> T getPropertyOrNull(Object bean, String name, Class<T> clazz) {
    try {
      return (T) getPropertyValue(bean, name);
    }
    catch (Exception ex) {
      return null;
    }
  }

  public static void setProperty(Object bean, String name, Object value) {
    FieldAndBean fieldAndBean = getFieldAndBean(bean, name);
    fieldAndBean.setFieldValue(value);
  }

  private static Object getPropertyValue(Object bean, String name) {
    FieldAndBean fieldAndBean = getFieldAndBean(bean, name);
    return fieldAndBean.getFieldValue();
  }

  private static FieldAndBean getFieldAndBean(Object bean, String name) {
    while(hasNestedProperty(name)) {
      String property = getOutermostProperty(name);
      bean = getPropertyValue(bean, property);
      name = removeOutermostProperty(name);
    }

    Optional<Field> optionalField = ReflectionUtils.findField(bean, name);
    if (optionalField.isPresent()) {
      Field f = optionalField.get();
      f.setAccessible(true);
      return new FieldAndBean(bean, f);
    }

    NoSuchFieldException noSuchFieldEx = new NoSuchFieldException("No field named " + name + " found on instance " + bean + " of class " + bean.getClass());
    throw new RuntimeException("Field not found", noSuchFieldEx);
  }

  private static class FieldAndBean {
    public Object bean;
    public Field field;

    public FieldAndBean(Object bean, Field field) {
      this.bean = bean;
      this.field = field;
    }

    public Object getFieldValue() {
      try {
        return field.get(bean);
      }
      catch (IllegalAccessException ex) {
        throw new RuntimeException("Could not get value for field " + field + " on bean " + bean + " of class " + bean.getClass(), ex);
      }
    }

    public void setFieldValue(Object value) {
      try {
        field.set(bean, value);
      }
      catch (IllegalAccessException ex) {
        throw new RuntimeException("Could not set value to " + value + " for field " + field + " on bean " + bean + " of class " + bean.getClass(), ex);
      }
    }
  }

  private static boolean hasNestedProperty(String name) {
    return name != null && name.contains(".");
  }

  private static String getOutermostProperty(String name) {
    return name.substring(0, name.indexOf("."));
  }

  private static String removeOutermostProperty(String name) {
    return name.substring(name.indexOf(".") + 1);
  }
}