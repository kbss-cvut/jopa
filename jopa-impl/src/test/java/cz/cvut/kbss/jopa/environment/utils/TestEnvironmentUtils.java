/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p/>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p/>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Value;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import static org.junit.Assert.assertEquals;

public final class TestEnvironmentUtils {

    private static Set<Class<?>> managedTypes;

    private TestEnvironmentUtils() {
        throw new AssertionError();
    }

    public static ObjectChangeSet createObjectChangeSet(Object original, Object clone, URI context) {
        return new ObjectChangeSetImpl(original, clone, context);
    }

    /**
     * Returns true if the specified assertions correspond to all properties in the {@code properties} parameter.
     * <p/>
     * Works for both string-based and typed properties.
     *
     * @param properties Expected properties
     * @param assertions Actual assertions
     * @return Equality status.
     */
    public static boolean assertionsCorrespondToProperties(Map properties, Map<Assertion, Set<Value<?>>> assertions) {
        if (properties.size() != assertions.size()) {
            return false;
        }
        for (Object entry : properties.entrySet()) {
            @SuppressWarnings("unchecked")
            Entry<?, Set<?>> e = (Entry<?, Set<?>>) entry;
            final Set<Value<?>> values = assertions
                    .get(Assertion.createPropertyAssertion(URI.create(e.getKey().toString()), false));
            if (values == null) {
                return false;
            }
            assertEquals(e.getValue().size(), values.size());
            for (Value<?> val : values) {
                if (!e.getValue().contains(val.getValue())) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Sets value of field {@code field} on the {@code target} object to the specified {@code mock}. </p>
     * <p/>
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
