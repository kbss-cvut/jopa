package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.model.annotations.Individual;
import cz.cvut.kbss.jopa.test.OWLClassR;
import cz.cvut.kbss.jopa.test.ObjectOneOfEnum;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Quad;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;

public abstract class EnumMappingTestRunner extends BaseRunner {

    public EnumMappingTestRunner(Logger logger,
                                 PersistenceFactory persistenceFactory,
                                 DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    public void persistSupportsMappingEnumValuesToObjectOneOfIndividuals() throws Exception {
        this.em = getEntityManager("persistSupportsMappingEnumValuesToObjectOneOfIndividuals", false);
        final OWLClassR instance = new OWLClassR(Generators.generateUri());
        instance.setObjectOneOf(Generators.getRandomItem(Arrays.asList(ObjectOneOfEnum.values())));
        transactional(() -> em.persist(instance));
        final String constantIri =
                ObjectOneOfEnum.class.getDeclaredField(instance.getObjectOneOf().name()).getAnnotation(
                        Individual.class).iri();
        verifyStatementsPresent(Collections.singletonList(
                new Quad(instance.getUri(), URI.create(Vocabulary.P_HAS_OBJECT_ONE_OF), URI.create(constantIri))), em);
    }

    @Test
    public void entityLifecycleSupportsMappingEnumValuesToObjectOneOfIndividuals() {
        this.em = getEntityManager("entityLifecycleSupportsMappingEnumValuesToObjectOneOfIndividuals", true);
        final OWLClassR instance = new OWLClassR(Generators.generateUri());
        instance.setObjectOneOf(ObjectOneOfEnum.ANNOTATION_PROPERTY);
        transactional(() -> em.persist(instance));

        transactional(() -> {
            final OWLClassR toUpdate = findRequired(OWLClassR.class, instance.getUri());
            assertEquals(ObjectOneOfEnum.ANNOTATION_PROPERTY, toUpdate.getObjectOneOf());
            toUpdate.setObjectOneOf(ObjectOneOfEnum.DATATYPE_PROPERTY);
        });

        final OWLClassR result = findRequired(OWLClassR.class, instance.getUri());
        assertEquals(ObjectOneOfEnum.DATATYPE_PROPERTY, result.getObjectOneOf());
    }
}
