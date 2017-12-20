package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.user.view.DbUserForm;
import gov.idaho.isp.saktrack.user.view.LdapUserForm;
import java.util.Optional;

/**
 * A "Preparer" is a service closely related to a Factory in that it produces
 * a desired object based on passed in minutiae, but the term "Preparer" denotes
 * the intent to be used by controller "prepare" methods (think Struts2 Preparable)
 * or @ModelAttribute methods (Spring MVC) in setting up an object to be used by
 * the controller's "action" method.
 */
public interface OrganizationUserPreparer {
  DbUserForm prepareVerifiedDbUserForm(Optional<Long> userId, Long organizationId, boolean hasCurrentPassword);
  DbUserForm prepareNewDbUserForm(Optional<Long> organizationId);
  LdapUserForm prepareVerifiedLdapUserForm(Optional<Long> userId, Long organizationId, String ldapUsername);
}
