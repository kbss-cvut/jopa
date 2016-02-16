/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.utils.Configuration;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.*;
import java.util.Map.Entry;

public final class TestEnvironmentUtils {

    private static Random random;

    private static Set<Class<?>> managedTypes;

    private TestEnvironmentUtils() {
        throw new AssertionError();
    }

    static {
        random = new Random();
    }

    public static ObjectChangeSet createObjectChangeSet(Object original, Object clone, URI context) {
        return new ObjectChangeSetImpl(original, clone, context);
    }

    public static int randomInt(int max) {
        return random.nextInt(max);
    }

    public static boolean arePropertiesEqual(Map<String, Set<String>> pOne,
                                             Map<String, Set<String>> pTwo) {
        if (pOne.size() != pTwo.size()) {
            return false;
        }
        for (Entry<String, Set<String>> e : pOne.entrySet()) {
            if (!pTwo.containsKey(e.getKey())) {
                return false;
            }
            final Set<String> set = pTwo.get(e.getKey());
            if (!e.getValue().equals(set)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Sets value of field {@code field} on the {@code target} object to the specified {@code mock}. </p>
     * <p>
     * This method also works for final fields. Note that in case static fields are set, it is the responsibility of the
     * client to reset the field to the original value in test cleanup.
     *
     * @param target Target instance, can be {@code null} for static fields
     * @param field  Field to set
     * @param mock   The new value
     */
    public static void setMock(Object target, Field field, Object mock) throws Exception {
        assert field != null;
        field.setAccessible(true);
        removeFinalModifier(field);
        field.set(target, mock);
    }

    private static void removeFinalModifier(Field field) throws Exception {
        // remove final modifier from field
        Field modifiersField = Field.class.getDeclaredField("modifiers");
        modifiersField.setAccessible(true);
        modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
    }

    public static Set<String> generateTypes(int count) {
        final Set<String> types = new HashSet<>(count);
        for (int i = 0; i < count; i++) {
            types.add("http://krizik.felk.cvut.cz/ontologies/jopa#type_" + i);
        }
        return types;
    }

    public static Map<String, Set<String>> generateProperties(int propCount, int valCount) {
        int valueCounter = 0;
        final Map<String, Set<String>> properties = new HashMap<>(propCount);
        for (int i = 0; i < propCount; i++) {
            final Set<String> values = new HashSet<>(valCount);
            properties.put("http://krizik.felk.cvut.cz/ontologies/jopa#property_" + i, values);
            boolean objectProperty = i % 2 != 0;
            for (int j = 0; j < valCount; j++) {
                if (objectProperty) {
                    values.add("http://krizik.felk.cvut.cz/ontologies/jopa#value_" + valueCounter++);
                } else {
                    values.add(j % 2 != 0 ? "value" + valueCounter++ : Integer.toString(valueCounter++));
                }
            }
        }
        return properties;
    }

    public static Set<Class<?>> getManagedTypes() {
        if (managedTypes == null) {
            initManagedTypes();
        }
        return Collections.unmodifiableSet(managedTypes);
    }

    private static void initManagedTypes() {
        managedTypes = new EntityLoader().discoverEntityClasses(new Configuration(
                Collections.singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa")));
    }
}
