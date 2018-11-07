package gov.idaho.isp.saktrack.service;

import java.util.function.Consumer;
import javax.persistence.EntityManager;

public interface NewTransactionExecutor {
  void doInNewTransaction(Consumer<EntityManager> work);
}