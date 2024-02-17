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

package gov.idaho.isp.saktrack.controller.lab;

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
import jakarta.servlet.http.HttpServletResponse;
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
public class LabDashboardController extends BaseOrganizationController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LabDashboardController(OrganizationUserRepository organizationUserRepository, SexualAssaultKitRepository sexualAssaultKitRepository) {
    super(organizationUserRepository);
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @GetMapping(value = "/lab/dashboard")
  public String dashboard(@CookieValue Optional<String> labPagingParams,
                          @RequestAttribute Organization organization,
                          LabDashboardPageRequest pageRequests,
                          Model model,
                          @RequestAttribute User user,
                          @RequestParam Map<String,String> requestParams,
                          HttpServletResponse response) throws UnsupportedEncodingException {

    if (requestParams.isEmpty() && labPagingParams.isPresent()) {
      return "redirect:/lab/dashboard" + labPagingParams.get();
    }
    model.addAttribute("organization", organization);

    model.addAttribute("newKits", findNewKits(organization, pageRequests.getNew()));
    model.addAttribute("incomingKits", findIncomingKits(organization, pageRequests.getIncoming()));
    model.addAttribute("inProcessKits", findInProcessKits(organization, pageRequests.getInProcess()));

    if (canAndShouldSeeUnverifiedUserMessage(organization, user)) {
      model.addAttribute("alerts", getText("new.user.verification"));
    }

    response.addCookie(CookieUtils.createEncoded("labPagingParams", pageRequests.getAsUrlParams()));
    return "lab/dashboard";
  }

  private Page<SexualAssaultKit> findNewKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findUnusedByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findIncomingKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findIncomingByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  private Page<SexualAssaultKit> findInProcessKits(Organization org, PageRequest pageReq) {
    Function<Pageable,Page<SexualAssaultKit>> getPageFn = pr -> sexualAssaultKitRepository.findUsedByOrganization(org.getId(), pr);
    return PagingUtils.getFirstPageIfRequestedIsBlank(getPageFn, pageReq);
  }

  public static class LabDashboardPageRequest {
    private static final int DEFAULT_PAGE_SIZE = 100;
    private int size = DEFAULT_PAGE_SIZE;

    private int newPage;
    private SortWrapper newSort;

    private int incomingPage;
    private SortWrapper incomingSort;

    private int inProcessPage;
    private SortWrapper inProcessSort;

    public int getSize() {
      return size;
    }

    public void setSize(int size) {
      this.size = size > 0 ? size : DEFAULT_PAGE_SIZE;
    }

    public int getNewPage() {
      return newPage;
    }

    public void setNewPage(int newPage) {
      this.newPage = newPage;
    }

    public SortWrapper getNewSort() {
      return newSort;
    }

    public void setNewSort(SortWrapper newSort) {
      this.newSort = newSort;
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

    public PageRequest getNew() {
      if (newSort != null) {
        return PageRequest.of(newPage, size, newSort.unwrap());
      }
      return PageRequest.of(newPage, size);
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

    public String getAsUrlParams() {
      StringBuilder sb = new StringBuilder();
      sb.append("?newPage=").append(newPage)
        .append("&newSort=").append(PagingUtils.getSingleOrderBy(newSort))
        .append("&incomingPage=").append(incomingPage)
        .append("&incomingSort=").append(PagingUtils.getSingleOrderBy(incomingSort))
        .append("&inProcessPage=").append(inProcessPage)
        .append("&inProcessSort=").append(PagingUtils.getSingleOrderBy(inProcessSort))
        .append("&size=").append(size);
      return sb.toString();
    }
  }
}
