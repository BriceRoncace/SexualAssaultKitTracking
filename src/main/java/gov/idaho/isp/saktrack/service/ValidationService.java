package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.LabDetails;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import gov.idaho.isp.saktrack.domain.LegalDetails;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.util.Set;

public interface ValidationService {
  Set<String> validateKit(SexualAssaultKit kit, Class... groups);
  Set<String> validateMedicalDetails(MedicalDetails details, Class... groups);
  Set<String> validateLawEnforcementDetails(LawEnforcementDetails details, Class... groups);
  Set<String> validateLegalDetails(LegalDetails details, Class... groups);
  Set<String> validateLabDetails(LabDetails details, Class... groups);
  Set<String> valiateChainOfCustodyEvent(ChainOfCustodyEvent event);
}
