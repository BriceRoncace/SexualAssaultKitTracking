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

package gov.idaho.isp.saktrack.service;

import java.util.function.Consumer;
import jakarta.persistence.EntityManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class NewTransactionExecutorImpl implements NewTransactionExecutor {
  private final EntityManager entityManager;

  public NewTransactionExecutorImpl(EntityManager entityManager) {
    this.entityManager = entityManager;
  }

  @Override
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void doInNewTransaction(Consumer<EntityManager> work) {
    work.accept(entityManager);
  }
}