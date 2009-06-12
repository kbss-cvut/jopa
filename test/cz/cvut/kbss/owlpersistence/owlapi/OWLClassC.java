package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.List;

import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLObjectProperty;

@OWLClass(uri = "http://OWLClassC")
public class OWLClassC {

	@Id
	private URI uri;

	@OWLObjectProperty(uri = "http://B-hasSequence")
	private List<OWLClassA> list;

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

	public void setList(List<OWLClassA> list) {
		this.list = list;
	}

	public List<OWLClassA> getList() {
		return list;
	}

}
