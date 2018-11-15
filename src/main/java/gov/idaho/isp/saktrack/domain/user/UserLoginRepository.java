package gov.idaho.isp.saktrack.domain.user;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface UserLoginRepository extends JpaRepository<UserLogin,Long>, JpaSpecificationExecutor<UserLogin> {
  
}
