package gov.idaho.isp.saktrack.domain.user;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface AbstractUserRepository extends JpaRepository<AbstractUser,Long>, JpaSpecificationExecutor<AbstractUser> {
  AbstractUser findByUsernameIgnoreCase(String username);
}