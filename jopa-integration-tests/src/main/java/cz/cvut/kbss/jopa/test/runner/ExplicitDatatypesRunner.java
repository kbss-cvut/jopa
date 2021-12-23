package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassX;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public abstract class ExplicitDatatypesRunner extends BaseRunner {

    private static final String VALUE = "P1Y";

    public ExplicitDatatypesRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void persistSupportsSavingValueOfAttributeWithExplicitDatatype() {
        this.em = getEntityManager("persistSupportsSavingValueOfAttributeWithExplicitDatatype", false);
        entityM.setExplicitDatatype(VALUE);
        transactional(() -> em.persist(entityM));

        verifyValueDatatype(URI.create(entityM.getKey()), Vocabulary.p_m_explicitDatatype, XSD.DURATION);
    }

    @Test
    public void readSupportsValuesWithExplicitDatatype() throws Exception {
        this.em = getEntityManager("readSupportsValuesWithExplicitDatatype", false);
        persistTestData(Arrays.asList(
                new Quad(URI.create(entityM.getKey()), URI.create(RDF.TYPE), URI.create(Vocabulary.C_OWL_CLASS_M)),
                new Quad(URI.create(entityM.getKey()), URI.create(Vocabulary.p_m_explicitDatatype),
                        new Literal(VALUE, XSD.DURATION))
        ), em);

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(VALUE, result.getExplicitDatatype());
    }

    @Test
    public void updatingExplicitDatatypeValuesIsSupported() {
        this.em = getEntityManager("updatingExplicitDatatypeValuesIsSupported", true);
        entityM.setExplicitDatatype(VALUE);
        persist(entityM);

        final String newValue = "PT55S";
        transactional(() -> {
            final OWLClassM toUpdate = findRequired(OWLClassM.class, entityM.getKey());
            toUpdate.setExplicitDatatype(newValue);
        });

        final OWLClassM cachedResult = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(newValue, cachedResult.getExplicitDatatype());
        em.getEntityManagerFactory().getCache().evictAll();
        em.clear();

        final OWLClassM result = findRequired(OWLClassM.class, entityM.getKey());
        assertEquals(newValue, result.getExplicitDatatype());
    }

    @Test
    public void pluralAttributesWithExplicitDatatypeAreSupported() {
        this.em = getEntityManager("pluralAttributesWithExplicitDatatypeAreSupported", false);
        final OWLClassX entity = new OWLClassX();
        final Set<String> values = IntStream.range(0, 5)
                                            .mapToObj(Integer::toString)
                                            .collect(Collectors.toSet());
        entity.setExplicitDatatypes(values);

        persist(entity);

        final OWLClassX result = findRequired(OWLClassX.class, entity.getUri());
        assertEquals(values, result.getExplicitDatatypes());
    }
}
