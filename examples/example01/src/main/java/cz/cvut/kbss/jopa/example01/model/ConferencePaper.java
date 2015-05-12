package cz.cvut.kbss.jopa.example01.model;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;

@OWLClass(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#ConferencePaper")
public class ConferencePaper {

	@Id(generated = true)
	private URI uri;
	
	@OWLDataProperty(iri = "http://uob.iodt.ibm.com/univ-bench-dl.owl#name")
	private String name;

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String toString() {
		return "[ConferencePaper " + name + ", uri = " + uri + "]";
	}
}
