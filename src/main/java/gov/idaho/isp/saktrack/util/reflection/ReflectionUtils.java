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

package gov.idaho.isp.saktrack.util.reflection;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public class ReflectionUtils {

  public static <T extends Class> T toClass(String clazz, T type) {
    try {
      return (T) Class.forName(clazz);
    }
    catch (ClassNotFoundException ex) {
      throw new RuntimeException("Could not find class by string [" + clazz + "]", ex);
    }
  }

  public static Class<?> toClass(String clazz) {
    try {
      return Class.forName(clazz);
    }
    catch (ClassNotFoundException ex) {
      throw new RuntimeException("Could not find class by string [" + clazz + "]", ex);
    }
  }

  public static <T> T newInstance(Class<T> clazz) {
    try {
      Constructor<T> constructor = clazz.getDeclaredConstructor();
      constructor.setAccessible(true);
      return constructor.newInstance();
    }
    catch (IllegalAccessException | InstantiationException | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
      throw new RuntimeException("Could not create new instance of class [" + clazz + "]", ex);
    }
  }

  public static Optional<Field> findField(Object bean, String field) {
    try {
      Field f = bean.getClass().getDeclaredField(field);
      return Optional.of(f);
    }
    catch (NoSuchFieldException ignore) {
      return getDeclaredFieldFromParents(bean, field);
    }
  }

  public static Optional<Method> findMethod(Object bean, String method, Class<?>... parameterTypes) {
    try {
      Method m = bean.getClass().getDeclaredMethod(method, parameterTypes);
      return Optional.of(m);
    }
    catch (NoSuchMethodException ignore) {
      return getDeclaredMethodFromParents(bean, method, parameterTypes);
    }
  }

  private static Optional<Field> getDeclaredFieldFromParents(Object bean, String field) {
    List<Field> fields = getDeclaredFieldsFromParents(bean.getClass());
    return fields.stream().filter((Field f) -> f.getName().equals(field)).findFirst();
  }

  private static List<Field> getDeclaredFieldsFromParents(Class clazz) {
    List<Field> fields = new ArrayList<>();
    while (clazz.getSuperclass() != null) {
      clazz = clazz.getSuperclass();
      fields.addAll(Arrays.asList(clazz.getDeclaredFields()));
    }
    return fields;
  }

  private static Optional<Method> getDeclaredMethodFromParents(Object bean, String method, Class<?>... parameterTypes) {
    List<Method> methods = getDeclaredMethodFromParents(bean.getClass());
    return methods.stream().filter((Method m) -> m.getName().equals(method) && Arrays.equals(m.getParameterTypes(), parameterTypes)).findFirst();
  }

  private static List<Method> getDeclaredMethodFromParents(Class clazz) {
    List<Method> methods = new ArrayList<>();
    while (clazz.getSuperclass() != null) {
      clazz = clazz.getSuperclass();
      methods.addAll(Arrays.asList(clazz.getDeclaredMethods()));
    }
    return methods;
  }
}

