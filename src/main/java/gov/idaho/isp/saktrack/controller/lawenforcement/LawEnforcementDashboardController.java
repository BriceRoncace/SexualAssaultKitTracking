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

package gov.idaho.isp.saktrack.controller.lawenforcement;

import gov.idaho.isp.saktrack.controller.BaseOrganizationController;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.CookieUtils;
import gov.idaho.isp.saktrack.util.PagingUtils;
import gov.idaho.isp.saktrack.util.SortWrapper;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import javax.servlet.http.HttpServletResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LawEnforcementDashboardController extends BaseOrganizationController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LawEnforcementDashboardController(OrganizationUserRepository organizationUserRepository, SexualAssaultKitRepository sexualAssaultKitRepository) {
    super(organizationUserRepository);
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @GetMapping("/law-enforcement/dashboard")
  public String dashboard(@CookieValue Optional<String> lePagingParams,
                          @RequestAttribute Organization organization,
                          LeDashboardPageRequest pageRequests,
                          Model model,
                          @RequestAttribute User user,
                          @RequestParam Map<String,String> requestParams,
                          HttpServletResponse response) throws UnsupportedEncodingException {

    if (requestParams.isEmpty() && lePagingParams.isPresent()) {
      return "redirect:/law-enforcement/dashboard" + lePagingParams.get();
    }

    model.addAttribute("organization", organization);

    model.addAttribute("incomingKits", findIncomingKits(organization, pageRequests.getIncoming()));
    model.addAttribute("inProcessKits", findInProcessKits(organization, pageRequests.getInProcess()));
    model.addAttribute("kitsInLab", findKitsInLab(organization, pageRequests.getInLab()));
    model.addAttribute("analyzedKits", findAnalyzedKits(organization, pageRequests.getAnalyzed()));
    model.addAttribute("unsubmittableKits", findUnsubmittableKits(organization, pageRequests.getUnsubmittable()));

    if (canAndShouldSeeUnverifiedUserMessage(organization, user)) {
      model.addAttribute("alerts", getText("new.user.verification"));
    }

    response.addCookie(CookieUtils.createEncoded("lePagingParams", pageRequests.getAsUrlParams()));
    return "law-enforcement/dashboard";
  }

  private Page<SexualAssaultKit> findIncomingKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findIncomingByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findInProcessKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findPendingSubmissionByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findKitsInLab(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findByInLabAndRequestedBy(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findAnalyzedKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findAnalyzedByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findUnsubmittableKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findUnsubmittableByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  public static class LeDashboardPageRequest {
    private static final int DEFAULT_PAGE_SIZE = 20;
    private int size = DEFAULT_PAGE_SIZE;

    private int incomingPage;
    private SortWrapper incomingSort;

    private int inProcessPage;
    private SortWrapper inProcessSort;

    private int inLabPage;
    private SortWrapper inLabSort;

    private int analyzedPage;
    private SortWrapper analyzedSort;

    private int unsubmittablePage;
    private SortWrapper unsubmittableSort;

    public int getSize() {
      return size;
    }

    public void setSize(int size) {
      this.size = size > 0 ? size : DEFAULT_PAGE_SIZE;
    }

    public int getIncomingPage() {
      return incomingPage;
    }

    public void setIncomingPage(int incomingPage) {
      this.incomingPage = incomingPage;
    }

    public SortWrapper getIncomingSort() {
      return incomingSort;
    }

    public void setIncomingSort(SortWrapper incomingSort) {
      this.incomingSort = incomingSort;
    }

    public int getInProcessPage() {
      return inProcessPage;
    }

    public void setInProcessPage(int inProcessPage) {
      this.inProcessPage = inProcessPage;
    }

    public SortWrapper getInProcessSort() {
      return inProcessSort;
    }

    public void setInProcessSort(SortWrapper inProcessSort) {
      this.inProcessSort = inProcessSort;
    }

    public int getInLabPage() {
      return inLabPage;
    }

    public void setInLabPage(int inLabPage) {
      this.inLabPage = inLabPage;
    }

    public SortWrapper getInLabSort() {
      return inLabSort;
    }

    public void setInLabSort(SortWrapper inLabSort) {
      this.inLabSort = inLabSort;
    }

    public int getAnalyzedPage() {
      return analyzedPage;
    }

    public void setAnalyzedPage(int analyzedPage) {
      this.analyzedPage = analyzedPage;
    }

    public SortWrapper getAnalyzedSort() {
      return analyzedSort;
    }

    public void setAnalyzedSort(SortWrapper analyzedSort) {
      this.analyzedSort = analyzedSort;
    }

    public int getUnsubmittablePage() {
      return unsubmittablePage;
    }

    public void setUnsubmittablePage(int unsubmittablePage) {
      this.unsubmittablePage = unsubmittablePage;
    }

    public SortWrapper getUnsubmittableSort() {
      return unsubmittableSort;
    }

    public void setUnsubmittableSort(SortWrapper unsubmittableSort) {
      this.unsubmittableSort = unsubmittableSort;
    }

    public PageRequest getIncoming() {
      if (incomingSort != null) {
        return PageRequest.of(incomingPage, size, incomingSort.unwrap());
      }
      return PageRequest.of(incomingPage, size);
    }

    public PageRequest getInProcess() {
      if (inProcessSort != null) {
        return PageRequest.of(inProcessPage, size, inProcessSort.unwrap());
      }
      return PageRequest.of(inProcessPage, size);
    }

    public PageRequest getInLab() {
      if (inLabSort != null) {
        return PageRequest.of(inLabPage, size, inLabSort.unwrap());
      }
      return PageRequest.of(inLabPage, size);
    }

    public PageRequest getAnalyzed() {
      if (analyzedSort != null) {
        return PageRequest.of(analyzedPage, size, analyzedSort.unwrap());
      }
      return PageRequest.of(analyzedPage, size);
    }

    public PageRequest getUnsubmittable() {
      if (unsubmittableSort != null) {
        return PageRequest.of(unsubmittablePage, size, unsubmittableSort.unwrap());
      }
      return PageRequest.of(unsubmittablePage, size);
    }

    public String getAsUrlParams() {
      StringBuilder sb = new StringBuilder();
      sb.append("?incomingPage=").append(incomingPage)
        .append("&incomingSort=").append(PagingUtils.getSingleOrderBy(incomingSort))
        .append("&inProcessPage=").append(inProcessPage)
        .append("&inProcessSort=").append(PagingUtils.getSingleOrderBy(inProcessSort))
        .append("&inLabPage=").append(inLabPage)
        .append("&inLabSort=").append(PagingUtils.getSingleOrderBy(inLabSort))
        .append("&analyzedPage=").append(analyzedPage)
        .append("&analyzedSort=").append(PagingUtils.getSingleOrderBy(analyzedSort))
        .append("&unsubmittablePage=").append(unsubmittablePage)
        .append("&unsubmittableSort=").append(PagingUtils.getSingleOrderBy(unsubmittableSort))
        .append("&size=").append(size);
      return sb.toString();
    }
  }
}
