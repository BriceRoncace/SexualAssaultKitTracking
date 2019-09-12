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