/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.util;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.AdminUser;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.LegalUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;

public class RoutingUtil {

  public static String getRoute(User user) {
    if (user.isAdmin()) {
      return "admin";
    }
    else if (user instanceof OrganizationUser) {
      return "org-users";
    }
    return null;
  }

  public static String getManageOrganizationOrDashboardView(User currentUser, Organization organization) {
    if (UserUtils.isAdminOrOrgAdmin(currentUser)) {
      return "redirect:/organization/" + organization.getId();
    }
    else {
      return "redirect:/" + currentUser.getType().getLabel() + "/dashboard";
    }
  }

  public static String getLoadKitView(SexualAssaultKit kit, User user) {
    if (User.Type.ADMIN == user.getType()) {
      return getLoadKitView(kit, (AdminUser) user);
    }
    else if (User.Type.LEGAL == user.getType()) {
      return getLoadKitView(kit, (LegalUser) user);
    }
    else {
      return getLoadKitView(kit, (OrganizationUser) user);
    }
  }

  private static String getLoadKitView(SexualAssaultKit kit, AdminUser admin) {
    return "redirect:/admin/edit?id=" + kit.getId();
  }

  private static String getLoadKitView(SexualAssaultKit kit, LegalUser attorney) {
    if (isKitReviewableBy(kit, attorney)) {
      if (kit.getStatus() == KitStatus.AWAITING_LEGAL_REVIEW) {
        return User.Type.LEGAL.getDevLabel() + "/" + EventType.RECEIVE.getViewName();
      }
      else {
        return getReadOnlyView(attorney, kit);
      }
    }
    else {
      return getChainOfCustodyView(kit);
    }
  }

  private static String getLoadKitView(SexualAssaultKit kit, OrganizationUser orgUser) {
    if (hasKitBeenTo(kit, orgUser.getOrganization())) {
      if (isKitAssignedToUsersOrganization(kit, orgUser)) {
        if (isUnusedKitInLabCustody(orgUser, kit)) {
          return orgUser.getType().getDevLabel() + "/" + EventType.CREATE.getViewName();
        }
        return getEditView(orgUser, kit);
      }

      if (isMedicalUserAndKitIsUnused(orgUser, kit)) {
        return getEditView(orgUser, kit);
      }

      return getReadOnlyView(orgUser, kit);
    }
    else {
      return getChainOfCustodyView(kit);
    }
  }

  private static boolean isUnusedKitInLabCustody(OrganizationUser orgUser, SexualAssaultKit kit) {
    return UserUtils.isLabUser(orgUser) && kit.getStatus() == KitStatus.UNUSED && EventType.SEND != kit.getCurrentCustody().getEventType();
  }

  private static boolean isMedicalUserAndKitIsUnused(OrganizationUser orgUser, SexualAssaultKit kit) {
    return UserUtils.isMedicalUser(orgUser) && kit.getMedicalDetails().getCollectionDate() == null;
  }

  private static boolean hasKitBeenTo(SexualAssaultKit kit, Organization org) {
    return kit.getChainOfCustody().stream().anyMatch(c -> org.equals(c.getFrom()) || org.equals(c.getTo()));
  }

  private static boolean isKitReviewableBy(SexualAssaultKit kit, LegalUser legalUser) {
    Organization legalOrg = legalUser.getOrganization();
    if (kit.getLegalDetails() != null && legalOrg.equals(kit.getLegalDetails().getReviewingOrganization())) {
      return true;
    }
    if (kit.getCurrentAssignment() != null && kit.getCurrentAssignment().getType() == OrganizationType.LAW_ENFORCEMENT && legalOrg.getJurisdiction().equals(kit.getCurrentAssignment().getJurisdiction())) {
      return true;
    }

    return false;
  }

  private static boolean isKitAssignedToUsersOrganization(SexualAssaultKit kit, OrganizationUser orgUser) {
    return kit.getCurrentAssignment() != null && kit.getCurrentAssignment().equals(orgUser.getOrganization());
  }

  private static String getChainOfCustodyView(SexualAssaultKit kit) {
    return "redirect:/timeline?serialNumber=" + kit.getSerialNumber();
  }

  private static String getReadOnlyView(OrganizationUser user, SexualAssaultKit kit) {
    return user.getType().getDevLabel()+ "/kit-readonly";
  }

  private static String getEditView(OrganizationUser user, SexualAssaultKit kit) {
    return user.getType().getDevLabel() + "/" + kit.getCurrentCustody().getEventType().getViewName();
  }
}
