package gov.idaho.isp.saktrack.service;

import java.util.function.Consumer;
import javax.persistence.EntityManager;
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