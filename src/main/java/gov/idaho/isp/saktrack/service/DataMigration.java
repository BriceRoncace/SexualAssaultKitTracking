package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import org.springframework.web.multipart.MultipartFile;

public interface DataMigration {
  Integer importNewOrganizations(MultipartFile csvFile) throws SexualAssaultKitTrackingException;
  Integer importNewOrganizationUsers(MultipartFile csvFile) throws SexualAssaultKitTrackingException;
}
