package cz.cvut.kbss.jopa.environment;

import java.lang.reflect.Field;
import java.net.URI;

import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassG")
public class OWLClassG {

	private static final String CLS_H_FIELD = "owlClassH";

	@Id
	private URI uri;

	@OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasH", fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	// @ParticipationConstraints({
	// @ParticipationConstraint(owlObjectIRI="http://new.owl#OWLClassA", min=1,
	// max=1)
	// })
	private OWLClassH owlClassH;

    public OWLClassG() {
    }

    public OWLClassG(URI uri) {
        this.uri = uri;
    }

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

	public void setOwlClassH(OWLClassH owlClassH) {
		this.owlClassH = owlClassH;
	}

	public OWLClassH getOwlClassH() {
		return owlClassH;
	}

	public static String getClassIri() {
		return OWLClassG.class.getAnnotation(OWLClass.class).iri();
	}

	public static Field getOwlClassHField() throws NoSuchFieldException, SecurityException {
		return OWLClassG.class.getDeclaredField(CLS_H_FIELD);
	}
}
