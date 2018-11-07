package gov.idaho.isp.saktrack.domain.user;

import org.springframework.data.jpa.repository.JpaRepository;

public interface AdminUserRepository extends JpaRepository<AdminUser,Long> {
  AdminUser findByUsernameIgnoreCase(String username);
}