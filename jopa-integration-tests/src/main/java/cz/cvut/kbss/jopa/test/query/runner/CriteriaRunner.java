package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.model.query.criteria.*;
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.util.Comparator;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

public abstract class CriteriaRunner extends BaseQueryRunner {

    protected CriteriaRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testSimpleFindAll() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        CriteriaFactory factory = getEntityManager().getCriteriaFactory();
        CriteriaQuery<OWLClassA> query = factory.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query, OWLClassA.class);

        final List<OWLClassA> result = tq.getResultList();

        assertEquals(expected.size(), result.size());
        for (OWLClassA a : result) {
            assertNotNull(a.getStringAttribute());
            assertTrue(expected.stream().anyMatch(aa -> aa.getUri().equals(a.getUri())));
        }
    }

    @Test
    public void testSimpleCount() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        CriteriaFactory factory = getEntityManager().getCriteriaFactory();
        CriteriaQuery<Long> query = factory.createQuery(Long.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(factory.count(root));

        final Object result = getEntityManager().createQuery(query, Long.class).getSingleResult();

        assertEquals(expected.size(), result);
    }

    @Test
    public void testFindByDataPropertyAttribute() {
        final OWLClassA expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaFactory factory = getEntityManager().getCriteriaFactory();
        CriteriaQuery<OWLClassA> query = factory.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        Predicate restriction = factory.equal(root.getAttr(OWLClassA_.stringAttribute),expected.getStringAttribute(),"en");
        query.select(root).where(restriction);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query, OWLClassA.class);

        final OWLClassA result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }

    //TODO - BAKALARKA - KONZULTACIA
    // nie je to trochu "pritiahnute za vlasy"?
    // unexpected = Generators.getRandomItem
    // assertNotEquals(unexpected.getUri(), item.getUri());
    // assertNotEquals(unexpected.getStringAttribute(), item.getStringAttribute());
//
//    @Test
//    public void testFindByDataNotPropertyAttribute() {
//        final OWLClassA unexpected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
//        final List<OWLClassA> result = getEntityManager()
//                .createQuery("SELECT a FROM OWLClassA a WHERE NOT a.stringAttribute = :str", OWLClassA.class)
//                .setParameter("str", unexpected.getStringAttribute(), "en").getResultList();
//        for (OWLClassA item : result) {
//            assertNotEquals(unexpected.getUri(), item.getUri());
//            assertNotEquals(unexpected.getStringAttribute(), item.getStringAttribute());
//        }
//    }
//
//    @Test
//    public void testFindByDataNotPropertyAttributeAndPropertyAttribute() {
//        final OWLClassT unexpected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
//        final int intThreshold = QueryTestEnvironment.getData(OWLClassT.class).size() / 2;
//        final List<OWLClassT> result = getEntityManager()
//                .createQuery("SELECT t FROM OWLClassT t WHERE NOT t.owlClassA = :a AND t.intAttribute < :intAtt",
//                        OWLClassT.class)
//                .setParameter("a", unexpected.getOwlClassA().getUri())
//                .setParameter("intAtt", intThreshold).getResultList();
//        assertFalse(result.isEmpty());
//        for (OWLClassT item : result) {
//            assertNotEquals(unexpected.getUri(), item.getUri());
//            assertTrue(intThreshold > item.getIntAttribute());
//        }
//    }
//
    @Test
    public void testFindByObjectPropertyAttribute() {
        final OWLClassD expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassD.class));
        CriteriaFactory factory = getEntityManager().getCriteriaFactory();
        CriteriaQuery<OWLClassD> query = factory.createQuery(OWLClassD.class);
        Root<OWLClassD> root = query.from(OWLClassD.class);
        Predicate restriction = factory.equal(root.getAttr("owlClassA"),expected.getOwlClassA().getUri());
        query.select(root).where(restriction);
        TypedQuery<OWLClassD> tq = getEntityManager().createQuery(query, OWLClassD.class);

        final OWLClassD result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getOwlClassA().getUri(), result.getOwlClassA().getUri());
    }
//
//    @Test
//    public void testFindByConjunctionOfAttributes() {
//        final OWLClassT sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
//        final List<OWLClassT> result = getEntityManager()
//                .createQuery("SELECT t FROM OWLClassT t WHERE t.owlClassA = :a AND t.intAttribute <= :intAtt",
//                        OWLClassT.class)
//                .setParameter("a", sample.getOwlClassA().getUri())
//                .setParameter("intAtt", sample.getIntAttribute()).getResultList();
//        assertFalse(result.isEmpty());
//        for (OWLClassT item : result) {
//            assertEquals(sample.getOwlClassA().getUri(), item.getOwlClassA().getUri());
//            assertThat(item.getIntAttribute(), lessThanOrEqualTo(sample.getIntAttribute()));
//        }
//    }
//
    @Test
    public void testOrderBy() {
        final List<OWLClassT> expected = QueryTestEnvironment.getData(OWLClassT.class);
        expected.sort(Comparator.comparing(OWLClassT::getIntAttribute));
        CriteriaFactory factory = getEntityManager().getCriteriaFactory();
        CriteriaQuery<OWLClassT> query = factory.createQuery(OWLClassT.class);
        Root<OWLClassT> root = query.from(OWLClassT.class);
        query.select(root).orderBy(factory.asc(root.getAttr("intAttribute")));
        TypedQuery<OWLClassT> tq = getEntityManager().createQuery(query, OWLClassT.class);

        final List<OWLClassT> result = tq.getResultList();

        assertEquals(expected.size(), result.size());
        for (OWLClassT t : result) {
            assertTrue(expected.stream().anyMatch(tt -> tt.getUri().equals(t.getUri())));
        }
    }
//
//    @Test
//    public void testFindByDisjunctionOfAttributes() {
//        final OWLClassT sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
//        final List<OWLClassT> result = getEntityManager()
//                .createQuery("SELECT t FROM OWLClassT t WHERE t.owlClassA = :a OR t.intAttribute <= :intAtt",
//                        OWLClassT.class)
//                .setParameter("a", sample.getOwlClassA().getUri())
//                .setParameter("intAtt", sample.getIntAttribute()).getResultList();
//        assertFalse(result.isEmpty());
//        for (OWLClassT item : result) {
//            boolean matches = item.getOwlClassA().getUri().equals(sample.getOwlClassA().getUri());
//            matches |= item.getIntAttribute() <= sample.getIntAttribute();
//            assertTrue(matches);
//        }
//    }
//
    @Test
    public void testFindByTransitiveAttributeValue() {
        final OWLClassD expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassD.class));
        CriteriaFactory factory = getEntityManager().getCriteriaFactory();
        CriteriaQuery<OWLClassD> query = factory.createQuery(OWLClassD.class);
        Root<OWLClassD> root = query.from(OWLClassD.class);
        Predicate restrictions = factory.equal(root.getAttr("owlClassA").getAttr("stringAttribute"), expected.getOwlClassA().getStringAttribute(),"en");
        query.select(root).where(restrictions);
        TypedQuery<OWLClassD> tq = getEntityManager().createQuery(query, OWLClassD.class);

        final OWLClassD result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getOwlClassA().getUri(), result.getOwlClassA().getUri());
    }

    @Test
    public void testFindByParameterExpression() {
        final OWLClassA expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaFactory factory = getEntityManager().getCriteriaFactory();
        CriteriaQuery<OWLClassA> query = factory.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        final ParameterExpression<String> strAtt = factory.parameter(String.class, "pOne");
        Predicate restriction = factory.equal(root.getAttr(OWLClassA_.stringAttribute), strAtt);
        query.select(root).where(restriction);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query, OWLClassA.class);
        tq.setParameter(strAtt, expected.getStringAttribute(), "en");

        final OWLClassA result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }

    @Test
    public void testFindByParameterExpressionUnnamed() {
        final OWLClassA expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaFactory factory = getEntityManager().getCriteriaFactory();
        CriteriaQuery<OWLClassA> query = factory.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        final ParameterExpression<String> strAtt = factory.parameter(String.class);
        Predicate restriction = factory.equal(root.getAttr(OWLClassA_.stringAttribute), strAtt);
        query.select(root).where(restriction);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query, OWLClassA.class);
        tq.setParameter(strAtt, expected.getStringAttribute(), "en");

        final OWLClassA result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }
}
