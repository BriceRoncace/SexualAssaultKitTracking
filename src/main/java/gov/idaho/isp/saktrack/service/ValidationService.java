package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.LabDetails;
import gov.idaho.isp.saktrack.LawEnforcementDetails;
import gov.idaho.isp.saktrack.LegalDetails;
import gov.idaho.isp.saktrack.MedicalDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import java.util.Set;

public interface ValidationService {
  Set<String> validateKit(SexualAssaultKit kit, Class... groups);
  Set<String> validateMedicalDetails(MedicalDetails details, Class... groups);
  Set<String> validateLawEnforcementDetails(LawEnforcementDetails details, Class... groups);
  Set<String> validateLegalDetails(LegalDetails details, Class... groups);
  Set<String> validateLabDetails(LabDetails details, Class... groups);
  Set<String> valiateChainOfCustodyEvent(ChainOfCustodyEvent event);
}
