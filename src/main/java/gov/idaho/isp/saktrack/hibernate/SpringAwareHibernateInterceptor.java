package gov.idaho.isp.saktrack.hibernate;

import java.io.Serializable;
import org.hibernate.EmptyInterceptor;
import org.hibernate.EntityMode;
import org.springframework.stereotype.Component;

/**
* A hibernate interceptor that uses Spring to instantiate objects.
*/
@Component
public class SpringAwareHibernateInterceptor extends EmptyInterceptor {
  private final EntityBeanLoader entityBeanLoader;

  public SpringAwareHibernateInterceptor(EntityBeanLoader entityBeanLoader) {
    this.entityBeanLoader = entityBeanLoader;
  }

  @Override
  public Object instantiate(String entityName, EntityMode entityMode, Serializable id) {
    return entityBeanLoader.loadEntityBean(entityName, id);
  }
}