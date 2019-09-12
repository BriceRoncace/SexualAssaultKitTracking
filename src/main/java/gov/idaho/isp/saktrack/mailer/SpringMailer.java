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

package gov.idaho.isp.saktrack.mailer;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import javax.activation.DataSource;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;

public class SpringMailer implements Mailer {
  private final JavaMailSender javaMailSender;

  private MimeType defaultMimeType = MimeType.HTML;
  private Charset defaultEncoding = StandardCharsets.UTF_8;
  private String defaultSender;
  private boolean inTestMode;
  private Collection<String> testDestinations = new ArrayList<>();

  public SpringMailer(String host) {
    this(host, false, new ArrayList<>());
  }

  public SpringMailer(JavaMailSender javaMailSender) {
    this(javaMailSender, false, new ArrayList<>());
  }

  public SpringMailer(String host, boolean inTestMode, Collection<String> testDestinations) {
    this.javaMailSender = initDefaultJavaMailSender(host, defaultEncoding);
    this.inTestMode = inTestMode;
    this.testDestinations = testDestinations;
  }

  public SpringMailer(String host, boolean inTestMode, String testDestinations) {
    this(host, inTestMode, MailUtils.splitAddresses(testDestinations));
  }

  public SpringMailer(JavaMailSender javaMailSender, boolean inTestMode, String testDestinations) {
    this(javaMailSender, inTestMode, MailUtils.splitAddresses(testDestinations));
  }

  public SpringMailer(JavaMailSender javaMailSender, boolean inTestMode, Collection<String> testDestinations) {
    this.javaMailSender = javaMailSender;
    this.inTestMode = inTestMode;
    this.testDestinations = testDestinations;
  }

  private JavaMailSender initDefaultJavaMailSender(String host, Charset defaultEncoding) {
    JavaMailSenderImpl sender = new JavaMailSenderImpl();
    sender.setHost(host);
    sender.setDefaultEncoding(host);
    sender.setDefaultEncoding(defaultEncoding.toString());
    return sender;
  }

  @Override
  public void send(MailMessage msg) {
    setEnvelopeFrom(msg.getEnvelopeFrom());

    javaMailSender.send(mimeMessage -> {
      MimeMessageHelper message = new MimeMessageHelper(mimeMessage, true, defaultEncoding.toString());

      if (msg.getSentDate() != null) {
        message.setSentDate(msg.getSentDate());
      }

      if (hasValue(getFrom(msg))) {
        message.setFrom(getFrom(msg));
      }
      if (hasValue(msg.getReplyTo())) {
        message.setReplyTo(msg.getReplyTo());
      }

      if (inTestMode) {
        message.setTo(asArray(getTestDestinations()));
      }
      else {
        if (hasValue(msg.getToEmails())) {
          message.setTo(asArray(msg.getToEmails()));
        }
        if (hasValue(msg.getCcEmails())) {
          message.setCc(asArray(msg.getCcEmails()));
        }
        if (hasValue(msg.getBccEmails())) {
          message.setBcc(asArray(msg.getBccEmails()));
        }
      }

      message.setSubject(msg.getSubject());
      message.setText(getText(msg), getMimeType(msg).isHtml());

      if (hasValue(msg.getAttachments())) {
        for (DataSource attachment : msg.getAttachments()) {
          message.addAttachment(attachment.getName(), attachment);
        }
      }

      if (hasValue(msg.getInlineElements())) {
        for (Map.Entry<String, DataSource> e : msg.getInlineElements().entrySet()) {
          message.addInline(e.getKey(), e.getValue());
        }
      }
    });
  }

  private String getText(MailMessage msg) {
    StringBuilder body = new StringBuilder();

    if (inTestMode) {
      body.append("***Note: Email sent from test environment.  In production, this email would have been sent to: ").append(msg.getToEmails());
      if (hasValue(msg.getCcEmails())) {
        body.append("; cc'd: ").append(msg.getCcEmails());
      }
      if (hasValue(msg.getBccEmails())) {
        body.append("; bcc'd: ").append(msg.getBccEmails());
      }

      String newLine = getMimeType(msg).getNewLine();
      body.append(newLine).append(newLine);
    }

    body.append(msg.getText());
    return body.toString();
  }

  private String getFrom(MailMessage msg) {
    return hasValue(msg.getFrom()) ? msg.getFrom() : getDefaultSender();
  }

  private MimeType getMimeType(MailMessage msg) {
    return msg.getMimeType() != null ? msg.getMimeType() : getDefaultMimeType();
  }

  private void setEnvelopeFrom(String envelopFrom) {
    if (hasValue(envelopFrom) && javaMailSender instanceof JavaMailSenderImpl) {
      ((JavaMailSenderImpl) javaMailSender).getJavaMailProperties().setProperty("mail.smtp.from", envelopFrom);
    }
  }

  private static String[] asArray(Collection<String> strings) {
    return strings == null ? null : strings.stream().toArray(String[]::new);
  }

  private static boolean hasValue(String string) {
    return string != null && !"".equals(string.trim());
  }

  private static boolean hasValue(Collection<?> collection) {
    return collection != null && !collection.isEmpty();
  }

  private static boolean hasValue(Map<?, ?> map) {
    return map != null && !map.isEmpty();
  }

  public Charset getDefaultEncoding() {
    return defaultEncoding;
  }

  public void setDefaultEncoding(Charset defaultEncoding) {
    this.defaultEncoding = defaultEncoding;
  }

  public MimeType getDefaultMimeType() {
    return defaultMimeType;
  }

  public void setDefaultMimeType(MimeType defaultMimeType) {
    this.defaultMimeType = defaultMimeType;
  }

  public String getDefaultSender() {
    return defaultSender;
  }

  public void setDefaultSender(String defaultSender) {
    this.defaultSender = defaultSender;
  }

  public boolean isInTestMode() {
    return inTestMode;
  }

  public void setInTestMode(boolean inTestMode) {
    this.inTestMode = inTestMode;
  }

  public Collection<String> getTestDestinations() {
    testDestinations = MailUtils.clean(testDestinations);
    return Collections.unmodifiableCollection(testDestinations);
  }

  public void setTestDestinations(String testDestinations) {
    this.testDestinations.clear();
    this.testDestinations.addAll(MailUtils.splitAddresses(testDestinations));
  }

  public void setTestDestinations(Collection<String> testDestinations) {
    this.testDestinations = testDestinations;
  }
}
