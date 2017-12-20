package gov.idaho.isp.saktrack.user.persistence;

import gov.idaho.isp.saktrack.user.AdminUser;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AdminUserRepository extends JpaRepository<AdminUser,Long> {
  AdminUser findByUsernameIgnoreCase(String username);
}