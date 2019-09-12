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

import java.io.Serializable;
import java.lang.reflect.Field;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.stereotype.Component;

@Component
public class EntityBeanLoader implements BeanFactoryAware {
  private static final String DEFAULT_ID_FIELD_NAME = "id";
  private String idFieldName = DEFAULT_ID_FIELD_NAME;
  private BeanFactory beanFactory;

  public String getIdFieldName() {
    return idFieldName;
  }

  public void setIdFieldName(String idFieldName) {
    this.idFieldName = idFieldName;
  }

  public Object loadEntityBean(String entityName, Serializable id) {
    String beanName = getBeanNameForEntity(entityName);
    if (isEntityBeanAvailable(beanName) && isEntityBeanAcceptable(beanName)) {
      Object bean = beanFactory.getBean(beanName);
      setId(bean, id);
      return bean;
    }
    return null;
  }

  protected boolean isEntityBeanAvailable(String beanName) {
    return beanFactory.containsBean(beanName);
  }

  /**
   * Ensures that the bean is prototype scoped, otherwise an IllegalStateException
   * is thrown in order to fail fast.  Override for custom behavior.
   */
  protected boolean isEntityBeanAcceptable(String beanName) {
    if (!beanFactory.isPrototype(beanName)) {
      throw new IllegalStateException(beanName + " is an invalid entity bean.  Entity beans should be prototype scoped.");
    }
    return true;
  }

  /**
   * Bean name derived from the class name (just the class name not fully-qualified
   * class name) in camel case.  For example, an entity name of
   * gov.idaho.isp.domain.AdminUser will result in a bean name of adminUser.
   *
   * Override this method for custom behavior.
   */
  protected String getBeanNameForEntity(String entityName) {
    if (entityName.contains(".")) {
      String className = entityName.substring(entityName.lastIndexOf(".")+1);
      return lowerCaseFirstLetter(className);
    }
    else if (entityName.length() > 1) {
      return lowerCaseFirstLetter(entityName);
    }
    else {
      return entityName;
    }
  }

  private static String lowerCaseFirstLetter(String str) {
    return str.substring(0,1).toLowerCase() + str.substring(1);
  }

  protected void setId(Object object, Serializable id) {
    Class<?> cls = object.getClass();
    try {
      Field field = getDeclaredFieldInClassOrParent(cls, getIdFieldName());
      field.setAccessible(true);
      field.set(object, id);
    }
    catch (IllegalAccessException | IllegalArgumentException | NoSuchFieldException | SecurityException ex) {
      throw new RuntimeException(ex);
    }
  }

  private Field getDeclaredFieldInClassOrParent(Class<?> cls, String fieldName) throws NoSuchFieldException {
    try {
      return cls.getDeclaredField(fieldName);
    }
    catch (NoSuchFieldException ex) {
      if (cls.getSuperclass() != null) {
        return getDeclaredFieldInClassOrParent(cls.getSuperclass(), fieldName);
      }
      throw ex;
    }
  }

  @Override
  public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
    this.beanFactory = beanFactory;
  }
}
