package gov.idaho.isp.saktrack.organization;

import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface OrganizationRepository extends JpaRepository<Organization,Long>, JpaSpecificationExecutor<Organization> {
  List<Organization> findByTypeOrderByNameAsc(OrganizationType type);
  List<Organization> findByJurisdictionOrderByNameAsc(Jurisdiction jurisdiction);
  List<Organization> findLegalByJusidictionId(Long jurisdictionId);
  List<Organization> findByEnabledOrderByNameAsc(boolean enabled);
  List<Organization> findAssignableOrganizations();
  Organization findByName(String name);
}