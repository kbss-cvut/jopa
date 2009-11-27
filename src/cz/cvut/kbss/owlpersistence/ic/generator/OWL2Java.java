package cz.cvut.kbss.owlpersistence.ic.generator;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChangeException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLStringLiteral;
import org.semanticweb.owlapi.util.OWLOntologyMerger;

import com.sun.codemodel.JAnnotationUse;
import com.sun.codemodel.JClassAlreadyExistsException;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JType;

import cz.cvut.kbss.owlpersistence.OWLDataProperty;
import cz.cvut.kbss.owlpersistence.OWLObjectProperty;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.ClassParticipationConstraint;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.DatatypeParticipationConstraint;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.LiteralParticipationConstraint;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.NamedIndividualParticipationConstraint;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.OWLPersistenceAnnotation;

public class OWL2Java {

	private static final Logger LOG = Logger
			.getLogger(OWL2Java.class.getName());

	private static final URI icProperty = URI
			.create("http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#isIntegrityConstraintFor");

	private Map<String, List<OWLPersistenceAnnotation>> ics = new HashMap<String, List<OWLPersistenceAnnotation>>();

	private OWLOntology merged;

	private Map<OWLClass, JDefinedClass> classes = new HashMap<OWLClass, JDefinedClass>();

	private void listContexts() {
		LOG.info("Available contexts: " + ics.keySet());
	}

	private void parseOntology(final String owlFileName,
			final String mappingFile) {
		try {
			// reader
			final File f = new File(owlFileName);
			final OWLOntologyManager m = OWLManager.createOWLOntologyManager();

			if (mappingFile != null) {
				LOG.info("Using mapping file '" + mappingFile + "'.");

				final Map<URI, URI> map = getMappings(mappingFile);
				m.addIRIMapper(new OWLOntologyIRIMapper() {

					@Override
					public URI getPhysicalURI(IRI arg0) {
						return map.get(arg0.toURI());
					}
				});
				LOG.info("Mapping file succesfully parsed.");
			}

			LOG.info("Loading ontology ...");
			m.loadOntology(IRI.create(f.toURI().toString()));
			merged = new OWLOntologyMerger(m).createMergedOntology(m, IRI
					.create("http://generated"));
			LOG.info("Done.");

			LOG.info("Parsing integrity constraints");

			final IntegrityConstraintParser icp = new IntegrityConstraintParserImpl();

			for (final OWLAxiom a : merged.getAxioms()) {
				for (final OWLAnnotation p : a.getAnnotations()) {
					if (!p.getProperty().getIRI().toURI().equals(icProperty)) {
						continue;
					}

					final OWLAnnotationValue icContext = p.getValue();

					if (!(icContext instanceof OWLStringLiteral)) {
						LOG
								.warning("Ignoring IC "
										+ a
										+ " for '"
										+ icContext
										+ "'. The context is not a valid string literal.");
						continue;
					}

					final String icContextName = ((OWLStringLiteral) icContext)
							.getLiteral();

					List<OWLPersistenceAnnotation> axioms = ics
							.get(icContextName);

					if (axioms == null) {
						axioms = new ArrayList<OWLPersistenceAnnotation>();
						ics.put(icContextName, axioms);
					}
					LOG.info("Found IC " + a + " for context " + icContextName);

					axioms.addAll(icp.parse(a, merged));
				}
			}
		} catch (OWLOntologyChangeException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		} catch (OWLOntologyCreationException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		} catch (UnsupportedICException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	private JDefinedClass ensureCreated(final JCodeModel cm, final OWLClass c) {
		if (!classes.containsKey(c)) {
			JDefinedClass cls;

			String name = "generated." + c.getIRI().getFragment();

			try {
				cls = cm._class(name);
			} catch (JClassAlreadyExistsException e) {
				cls = cm._getClass(name);
			}
			classes.put(c, cls);

			return cls;
		} else {
			return classes.get(c);
		}
	}

	private void generateSources() {
		LOG.info("Generating sources ...");
		final JCodeModel cm = new JCodeModel();

		try {

			final Map<org.semanticweb.owlapi.model.OWLClass, JDefinedClass> classes = new HashMap<org.semanticweb.owlapi.model.OWLClass, JDefinedClass>();
			// JDefinedClass dc = cm._class("foo.Bar");
			// JMethod m = dc.method(0, int.class, "foo");
			// m.body()._return(JExpr.lit(5));

			for (final String contextName : ics.keySet()) {
				for (final OWLPersistenceAnnotation a : ics.get(contextName)) {
					// for (final org.semanticweb.owlapi.model.OWLClass c : a
					// .getClassesInSignature()) {
					// if (!classes.containsKey(c)) {
					// classes.put(c, cm._class("generated."
					// + c.getIRI().getFragment()));
					// }
					// }

					if (a instanceof ClassParticipationConstraint) {
						final ClassParticipationConstraint cpc = (ClassParticipationConstraint) a;

						final JDefinedClass subj = ensureCreated(cm, cpc
								.getSubject());
						final JDefinedClass obj = ensureCreated(cm, cpc
								.getObject());

						final JMethod m = subj.method(0, obj, "get"
								+ cpc.getPredicate().getIRI().getFragment());
						final JAnnotationUse use = m
								.annotate(OWLObjectProperty.class);

						use
								.param("iri", cpc.getPredicate().getIRI()
										.toString());

						// TODO cardinalities
					} else if (a instanceof NamedIndividualParticipationConstraint) {
						throw new UnsupportedICException("NOT IMPLEMENTED YET");
					} else if (a instanceof DatatypeParticipationConstraint) {
						final DatatypeParticipationConstraint cpc = (DatatypeParticipationConstraint) a;

						final JDefinedClass subj = ensureCreated(cm, cpc
								.getSubject());

						final JType type;

						final OWLDatatype dt = cpc.getObject();

						if (dt.isBuiltIn()) {
							if (dt
									.getIRI()
									.toString()
									.equals(
											"http://www.w3.org/2000/01/rdf-schema#Literal")) {
								throw new UnsupportedICException(
										"LITERAL constants not supported: "
												+ cpc.getSubject() + " : "
												+ cpc.getPredicate() + " : "
												+ cpc.getObject());
							} else {
								switch (dt.getBuiltInDatatype()) {
								case XSD_STRING:
								case RDF_XML_LITERAL:
									type = cm._ref(String.class);
									break;
								case XSD_INT:
								case XSD_INTEGER:
									type = cm.INT;
									break;
								case XSD_DOUBLE:
									type = cm.DOUBLE;
									break;
								case XSD_FLOAT:
									type = cm.FLOAT;
									break;
								case XSD_BOOLEAN:
									type = cm.BOOLEAN;
									break;
								case XSD_DATE_TIME:
									type = cm._ref(Date.class);
									break;
								case XSD_SHORT:
									type = cm.SHORT;
									break;
								case XSD_LONG:
									type = cm.LONG;
									break;
								case XSD_ANY_URI:
									type = cm._ref(URI.class);
									break;
								default:
									throw new UnsupportedICException(
											"Unsupported data type: " + dt);
								}
							}
						} else {
							throw new UnsupportedICException(
									"Unsupported data type: " + dt);

						}
						final JMethod m = subj.method(0, type, "get"
								+ cpc.getPredicate().getIRI().getFragment());
						m.annotate(OWLDataProperty.class);
						// TODO cardinalities
					} else if (a instanceof LiteralParticipationConstraint) {
						throw new UnsupportedICException("NOT IMPLEMENTED YET");
					} else {
						throw new UnsupportedICException("Uknown IC type: "
								+ a.getClass().getName());
					}
				}
				File file = new File("classes-" + contextName);
				file.mkdirs();
				cm.build(file);
			}
		} catch (UnsupportedICException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	public static Map<URI, URI> getMappings(final String mappingFile) {
		final Map<URI, URI> map = new HashMap<URI, URI>();
		String line = null;
		final File mf = new File(mappingFile);
		final File defaultDir = mf.getParentFile();
		BufferedReader r;
		try {
			r = new BufferedReader(new InputStreamReader(
					new FileInputStream(mf)));
			while ((line = r.readLine()) != null) {
				final StringTokenizer t = new StringTokenizer(line, ">");
				if (t.countTokens() != 2) {
					System.out
							.println("Ignoring line '" + line
									+ "' - invalid number of tokens="
									+ t.countTokens());
					continue;
				}

				final String uriName = t.nextToken().trim();
				final String fileName = t.nextToken().trim();
				final File actualFile = (new File(fileName).isAbsolute()) ? new File(
						fileName)
						: new File(defaultDir, fileName);

				map.put(URI.create(uriName), actualFile.toURI());
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		return map;
	}

	private enum COMMAND {
		help, list, transform;
	}

	public static void main(String[] args) throws OWLOntologyChangeException {
		if (args.length == 0) {
			System.out
					.println("Syntax: OWL2Java <command> <args>. Run 'OWL2Java help' for more details");
			return;
		}

		LOG.info("OWL IC processor");

		final OWL2Java oj = new OWL2Java();
		final COMMAND c;

		c = COMMAND.valueOf(args[0]);
		switch (c) {
		case help:
			System.out.println("Available commands : "
					+ Arrays.asList(COMMAND.values()));
			break;
		case list:
			oj.parseOntology(args[1], args[2]);
			oj.listContexts();
			break;
		case transform:
			oj.parseOntology(args[1], args[2]);
			oj.generateSources();
			break;
		default:
			System.out.println("Unknown command '" + args[0]
					+ "', try 'OWL2Java help.'");
		}
	}
}
