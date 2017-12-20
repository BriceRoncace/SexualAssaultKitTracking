package gov.idaho.isp.saktrack.user.persistence;

import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

public interface OrganizationUserRepository extends JpaRepository<AbstractOrganizationUser,Long> {
  AbstractOrganizationUser findByUsernameIgnoreCase(String username);
  List<AbstractOrganizationUser> findByOrganizationIdOrderByDisplayNameAsc(Long id);
  List<AbstractOrganizationUser> findUnverifiedUserByOrganization(Long id);

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  @Query("FROM AbstractOrganizationUser WHERE id = ?1")
  AbstractOrganizationUser findOneInNewTransaction(Long id);
}
