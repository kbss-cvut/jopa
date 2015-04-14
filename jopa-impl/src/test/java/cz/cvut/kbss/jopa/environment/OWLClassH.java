package cz.cvut.kbss.jopa.environment;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassH")
public class OWLClassH {

	private static final String CLS_A_FIELD = "owlClassA";

	@Id
	private URI uri;

	@OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA", fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	// @ParticipationConstraints({
	// @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
	// max=1)
	// })
	private OWLClassA owlClassA;

	/**
	 * @param uri
	 *            the uri to set
	 */
	public void setUri(URI uri) {
		this.uri = uri;
	}

	/**
	 * @return the uri
	 */
	public URI getUri() {
		return uri;
	}

	public void setOwlClassA(OWLClassA owlClassA) {
		this.owlClassA = owlClassA;
	}

	public OWLClassA getOwlClassA() {
		return owlClassA;
	}

	public static String getClassIri() {
		return OWLClassH.class.getAnnotation(OWLClass.class).iri();
	}

	public static Field getOwlClassAField() throws NoSuchFieldException, SecurityException {
		return OWLClassH.class.getDeclaredField(CLS_A_FIELD);
	}
}
