package gov.idaho.isp.saktrack.domain.jurisdiction;

import org.springframework.data.jpa.repository.JpaRepository;

public interface JurisdictionRepository extends JpaRepository<Jurisdiction,Long> {
  Jurisdiction findByName(String name);
}
