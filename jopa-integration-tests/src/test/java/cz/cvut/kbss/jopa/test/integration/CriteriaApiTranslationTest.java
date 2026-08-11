package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Path;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.test.OWLClassG;
import cz.cvut.kbss.jopa.test.OWLClassH;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Iterator;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalToCompressingWhiteSpace;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class CriteriaApiTranslationTest extends IntegrationTestBase {

    @Mock
    private Statement statementMock;
    @Mock
    private ResultSet resultSetMock;
    @Mock
    private Iterator<ResultRow> iteratorMock;

    /**
     * Bug #462
     */
    @Test
    public void testNestedIdentifierReference() throws Exception {
        when(connectionMock.createStatement()).thenReturn(statementMock);
        when(resultSetMock.iterator()).thenReturn(iteratorMock);
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        final CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery<OWLClassG> query = cb.createQuery(OWLClassG.class);
        Root<OWLClassG> root = query.from(OWLClassG.class);
        final Path<OWLClassH> hPath = root.getAttr("owlClassH");
        final Path<URI> idAPath = hPath.getAttr("owlClassA").getAttr("uri");
        final URI filterValue = Generators.generateUri();

        query.select(root).where(cb.equal(idAPath, filterValue));
        em.createQuery(query).getResultList();
        final ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        verify(statementMock).executeQuery(captor.capture());
        assertThat(captor.getValue(), equalToCompressingWhiteSpace("SELECT ?x WHERE { ?x a " + strUri(Vocabulary.C_OWL_CLASS_G) + " . " +
                "?x " + strUri(Vocabulary.P_HAS_H) + " ?owlClassH . " +
                "?owlClassH " + strUri(Vocabulary.P_HAS_OWL_CLASS_A) + " " + strUri(filterValue) + " . } "));
    }

    private static String strUri(Object uri) {
        return "<" + uri.toString() + ">";
    }
}
