/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.query;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.*;
import java.util.Map.Entry;

public final class QueryTestEnvironment {

    private static final Logger LOG = LoggerFactory.getLogger(QueryTestEnvironment.class);

    private static final int ITEM_COUNT = 10;

    private static final String BASE_A = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA_";
    private static final String TYPE_A = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeA";
    private static final String BASE_B = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB_";
    private static final String BASE_D = "http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD_";

    private static final URI NULL_CONTEXT = URI.create("http://NullContext");

    private static Map<Class<?>, List<?>> data;
    private static final Map<URI, Map<Class<?>, List<?>>> dataByContext = new HashMap<>();

    private QueryTestEnvironment() {
        // Private constructor to prevent instantiation
    }

    /**
     * Generates and persists test data into the default context of the
     * specified entity manager.
     *
     * @param em EntityManager
     */
    public static void generateTestData(EntityManager em) {
        assert em != null;
        final Map<Class<?>, List<?>> map = generate();
        LOG.debug("Persisting test data...");
        persistIntoContext(em, map, null);
        data = map;
    }

    /**
     * Generates and persists test data into the specified contexts.
     * <p>
     * This method distributes the data approximately uniformly into all the
     * specified contexts.
     *
     * @param em       EntityManager
     * @param contexts A collection of target contexts
     */
    public static void generateTestData(EntityManager em, Collection<URI> contexts) {
        assert em != null;
        assert contexts != null && !contexts.isEmpty();
        final Map<Class<?>, List<?>> map = generate();
        LOG.debug("Persisting test data...");
        final int contextCount = contexts.size();
        final Map<URI, Map<Class<?>, List<?>>> contextMap = new HashMap<>();
        for (Entry<Class<?>, List<?>> e : map.entrySet()) {
            final List<?> dataLst = e.getValue();
            final int sublistSize = dataLst.size() / contextCount;
            int sublistStart = 0;
            for (URI ctx : contexts) {
                if (!contextMap.containsKey(ctx)) {
                    contextMap.put(ctx, new HashMap<>());
                }
                final List<?> sublist = dataLst.subList(sublistStart, sublistStart + sublistSize);
                contextMap.get(ctx).put(e.getKey(), sublist);
                sublistStart += sublistSize;
            }
        }
        contextMap.forEach((ctx, lst) -> persistIntoContext(em, lst, ctx));
        data = map;
    }

    private static void persistIntoContext(EntityManager em, Map<Class<?>, List<?>> data, URI context) {
        final EntityDescriptor desc = new EntityDescriptor(context);
        try {
            em.getTransaction().begin();
            for (List<?> l : data.values()) {
                for (Object o : l) {
                    em.persist(o, desc);
                }
            }
            em.getTransaction().commit();
            if (context == null) {
                context = NULL_CONTEXT;
            }
            dataByContext.put(context, data);
        } catch (RuntimeException e) {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            throw e;
        }
    }

    /**
     * Get all current test data.
     *
     * @return Map of test data
     */
    public static Map<Class<?>, List<?>> getData() {
        return data;
    }

    /**
     * Get a list of test instances of the specified class.
     *
     * @param cls The class
     * @return List of test data of the specified class
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> getData(Class<T> cls) {
        assert cls != null;
        return (List<T>) data.get(cls);
    }

    /**
     * Gets data from the specified context and of the specified type
     *
     * @param context Context URI, null is permitted
     * @param cls     Data type
     * @return List of data or an empty list
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> getDataByContext(URI context, Class<T> cls) {
        assert cls != null;
        if (context == null) {
            context = NULL_CONTEXT;
        }
        if (!dataByContext.containsKey(context)) {
            return Collections.emptyList();
        }
        final Map<Class<?>, List<?>> contextData = dataByContext.get(context);
        if (!contextData.containsKey(cls)) {
            return Collections.emptyList();
        }
        return (List<T>) contextData.get(cls);
    }

    private static Map<Class<?>, List<?>> generate() {
        LOG.debug("Generating test data...");
        final Map<Class<?>, List<?>> m = new HashMap<>();
        final List<OWLClassA> aa = generateOwlClassAInstances();
        m.put(OWLClassA.class, aa);
        m.put(OWLClassB.class, generateOwlClassBInstances());
        m.put(OWLClassD.class, generateOwlClassDInstances(aa));
        m.put(OWLClassE.class, generateOwlClassEInstances());
        m.put(OWLClassJ.class, generateOwlClassJInstances(aa));
        m.put(OWLClassT.class, generateOwlClassTInstances(aa));
        return m;
    }

    private static List<OWLClassA> generateOwlClassAInstances() {
        final List<OWLClassA> lst = new ArrayList<>(ITEM_COUNT);
        int randomNum = Generators.randomInt(1000);
        for (int i = 0; i < ITEM_COUNT; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create(BASE_A + randomNum));
            a.setStringAttribute("stringAttribute" + randomNum);
            final Set<String> s = new HashSet<>();
            s.add(TYPE_A);
            a.setTypes(s);
            lst.add(a);
            randomNum++;
        }
        return lst;
    }

    private static List<OWLClassB> generateOwlClassBInstances() {
        final List<OWLClassB> lst = new ArrayList<>(ITEM_COUNT);
        int randomNum = Generators.randomInt(1000);
        for (int i = 0; i < ITEM_COUNT; i++) {
            final OWLClassB b = new OWLClassB();
            b.setUri(URI.create(BASE_B + randomNum));
            b.setStringAttribute("strAtt" + randomNum);
            lst.add(b);
            randomNum++;
        }
        return lst;
    }

    private static List<OWLClassD> generateOwlClassDInstances(List<OWLClassA> aList) {
        final List<OWLClassD> lst = new ArrayList<>(ITEM_COUNT);
        int randomNum = Generators.randomInt(1000);
        for (int i = 0; i < ITEM_COUNT; i++) {
            final OWLClassD d = new OWLClassD();
            d.setUri(URI.create(BASE_D + randomNum));
            d.setOwlClassA(aList.get(i));
            lst.add(d);
            randomNum++;
        }
        return lst;
    }

    private static List<OWLClassE> generateOwlClassEInstances() {
        final List<OWLClassE> lst = new ArrayList<>(ITEM_COUNT);
        for (int i = 0; i < ITEM_COUNT; i++) {
            final OWLClassE e = new OWLClassE();
            // Auto-generated id
            e.setStringAttribute("eStr" + i);
            lst.add(e);
        }
        return lst;
    }

    private static List<OWLClassT> generateOwlClassTInstances(List<OWLClassA> aList) {
        final List<OWLClassT> lst = new ArrayList<>(ITEM_COUNT);
        for (int i = 0; i < ITEM_COUNT; i++) {
            final OWLClassT t = new OWLClassT();
            t.setIntAttribute(i);
            t.setName("tInstance " + i);
            t.setDescription("Description of tInstance" + i);
            t.setOwlClassA(aList.get(Generators.randomInt(aList.size())));
            lst.add(t);
        }
        return lst;
    }

    private static List<OWLClassJ> generateOwlClassJInstances(List<OWLClassA> aList) {
        final List<OWLClassJ> lst = new ArrayList<>(ITEM_COUNT);
        for (int i = 0; i < ITEM_COUNT; i++) {
            final OWLClassJ inst = new OWLClassJ(Generators.generateUri());
            inst.setOwlClassA(new HashSet<>());
            for (int j = 0; j < 3; j++) {
                inst.getOwlClassA().add(Generators.getRandomItem(aList));
            }
            lst.add(inst);
        }
        return lst;
    }
}
