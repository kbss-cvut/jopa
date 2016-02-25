/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.test.OWLClassA;

import java.net.URI;
import java.util.*;

/**
 * Generators of test data.
 *
 * @author ledvima1
 */
public abstract class Generators {

    private static final int DEFAULT_SIZE = 5;
    private static final Set<String> TYPES = getTypes();

    private Generators() {
        // Private constructor
    }

    public static List<OWLClassA> createSimpleList() {
        return createSimpleList(DEFAULT_SIZE);
    }

    public static List<OWLClassA> createReferencedList() {
        return createReferencedList(DEFAULT_SIZE);
    }

    public static Set<OWLClassA> createSimpleSet() {
        return createSimpleSet(DEFAULT_SIZE);
    }

    public static List<OWLClassA> createSimpleList(int size) {
        assert size > 0;
        final List<OWLClassA> lst = new ArrayList<>(size);
        generateInstances(lst, "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityASimple",
                size);
        return lst;
    }

    public static List<OWLClassA> createReferencedList(int size) {
        assert size > 0;
        final List<OWLClassA> lst = new ArrayList<>(size);
        generateInstances(lst,
                "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityAReferenced", size);
        return lst;
    }

    public static Map<String, Set<String>> createProperties() {
        return createProperties(DEFAULT_SIZE);
    }

    public static Map<String, Set<String>> createProperties(int size) {
        assert size > 0;
        final Map<String, Set<String>> m = new HashMap<>(size);
        int counter = TestEnvironmentUtils.randomInt(1000);
        for (int i = 0; i < size; i++) {
            final Set<String> value = new HashSet<>(4);
            for (int j = 0; j < size; j++) {
                value.add("http://krizik.felk.cvut.cz/ontologies/jopa/tests/ObjectPropertyValue_" + j + "_"
                        + counter);
            }
            m.put("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#property" + counter, value);
            counter++;

        }
        return m;
    }

    public static Set<OWLClassA> createSimpleSet(int size) {
        assert size > 0;
        final Set<OWLClassA> set = new HashSet<>(size);
        generateInstances(set, "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityASimpleSet",
                size);
        return set;
    }

    private static void generateInstances(Collection<OWLClassA> col, String uriBase, int size) {
        assert size > 0;
        int counter = TestEnvironmentUtils.randomInt(1000);
        for (int i = 0; i < size; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create(uriBase + counter));
            a.setStringAttribute("stringAttributeeee" + counter);
            counter++;
            a.setTypes(TYPES);
            col.add(a);
        }
    }

    private static Set<String> getTypes() {
        final Set<String> types = new HashSet<>(3);
        types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA");
        types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX");
        types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassZ");
        return types;
    }
}
