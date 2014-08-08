package cz.cvut.kbss.jopa.test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;

@OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassF")
public class OWLClassF {

	private static final String STR_ATT_FIELD = "secondStringAttribute";
	private static final String SET_FIELD = "simpleSet";

	@Id
	private URI uri;

	@OWLDataProperty(iri = "http://F-secondStringAttribute", inferred = true)
	private String secondStringAttribute;

	@Sequence(type = SequenceType.simple, ObjectPropertyHasNextIRI = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#F-hasSimpleNext")
	@OWLObjectProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#F-hasSimpleSequence")
	private Set<OWLClassA> simpleSet;

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public String getSecondStringAttribute() {
		return secondStringAttribute;
	}

	public void setSecondStringAttribute(String secondStringAttribute) {
		this.secondStringAttribute = secondStringAttribute;
	}

	public Set<OWLClassA> getSimpleSet() {
		if (simpleSet == null) {
			this.simpleSet = new HashSet<OWLClassA>();
		}
		return simpleSet;
	}

	public void setSimpleSet(Set<OWLClassA> simpleSet) {
		this.simpleSet = simpleSet;
	}

	public static String getClassIri() {
		return OWLClassF.class.getAnnotation(OWLClass.class).iri();
	}

	public static Field getStrAttField() throws NoSuchFieldException, SecurityException {
		return OWLClassF.class.getDeclaredField(STR_ATT_FIELD);
	}

	public static Field getSimpleSetField() throws NoSuchFieldException, SecurityException {
		return OWLClassF.class.getDeclaredField(SET_FIELD);
	}

	@Override
	public String toString() {
		String out = "OWLClassF: uri = " + uri;
		out += ", secondStringAttribute = " + secondStringAttribute;
		return out;
	}
}
