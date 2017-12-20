package gov.idaho.isp.saktrack.audit;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface KitAuditRepository extends JpaRepository<KitAudit, Long> {
  List<KitAudit> findByKitIdOrderByModifiedDesc(Long id);
}