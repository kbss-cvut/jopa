package cz.cvut.kbss.owlpersistence.ic.generator;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChangeException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLStringLiteral;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.semanticweb.owlapi.util.OWLOntologyMerger;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import com.sun.codemodel.JClass;
import com.sun.codemodel.JClassAlreadyExistsException;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JDocComment;
import com.sun.codemodel.JExpr;
import com.sun.codemodel.JFieldVar;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JType;
import com.sun.codemodel.JVar;

import cz.cvut.kbss.owlpersistence.ic.internalmodel.ClassParticipationConstraint;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.DatatypeParticipationConstraint;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.LiteralParticipationConstraint;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.NamedIndividualParticipationConstraint;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.OWLPersistenceAnnotation;
import cz.cvut.kbss.owlpersistence.ic.internalmodel.SubClassOfSpecification;
import cz.cvut.kbss.owlpersistence.model.annotations.Id;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLDataProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.owlpersistence.model.annotations.Types;
import cz.cvut.kbss.owlpersistence.owlapi.fresh.DatatypeTransformer;

public class OWL2JavaTransformer {

	private static final Logger LOG = Logger
			.getLogger(OWL2JavaTransformer.class.getName());

	private static final org.semanticweb.owlapi.model.IRI icProperty = org.semanticweb.owlapi.model.IRI
			.create("http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#isIntegrityConstraintFor");

	private Map<String, List<OWLPersistenceAnnotation>> ics = new HashMap<String, List<OWLPersistenceAnnotation>>();

	private OWLOntology merged;

	private Map<OWLClass, JDefinedClass> classes = new HashMap<OWLClass, JDefinedClass>();

	private Map<org.semanticweb.owlapi.model.OWLEntity, JFieldVar> entities = new HashMap<OWLEntity, JFieldVar>();

	private OWLReasoner r;

	public Collection<String> listContexts() {
		return ics.keySet();
	}

	private void setupOWLReasoner() {
		// this.r = new OWLAPIIdentityReasonerFactory().createReasoner(merged);
		this.r = new StructuralReasonerFactory().createReasoner(merged);
	}

	public OWL2JavaTransformer(final String owlOntologyName,
			final String mappingFile) {
		try {
			// reader
			final OWLOntologyManager m = OWLManager.createOWLOntologyManager();

			if (mappingFile != null) {
				LOG.info("Using mapping file '" + mappingFile + "'.");

				final Map<URI, URI> map = getMappings(mappingFile);
				m.addIRIMapper(new OWLOntologyIRIMapper() {

					@Override
					public org.semanticweb.owlapi.model.IRI getDocumentIRI(
							org.semanticweb.owlapi.model.IRI ontologyIRI) {
						final URI value = map.get(ontologyIRI.toURI());

						if (value == null) {
							return null;
						} else {
							return org.semanticweb.owlapi.model.IRI
									.create(value);
						}
					}
				});
				LOG.info("Mapping file succesfully parsed.");
			}

			LOG.info("Loading ontology " + owlOntologyName + " ... ");
			m.setSilentMissingImportsHandling(true);
			m.loadOntology(org.semanticweb.owlapi.model.IRI
					.create(owlOntologyName));
			merged = new OWLOntologyMerger(m)
					.createMergedOntology(m, org.semanticweb.owlapi.model.IRI
							.create("http://generated"));

			setupOWLReasoner();

			LOG.info("Parsing integrity constraints");
			final IntegrityConstraintParser icp = new IntegrityConstraintParserImpl();

			for (final OWLAxiom a : merged.getAxioms()) {
				for (final OWLAnnotation p : a.getAnnotations()) {
					if (!p.getProperty().getIRI().equals(icProperty)) {
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
					LOG.config("Found IC " + a + " for context "
							+ icContextName);

					axioms.addAll(icp.parse(a, r, merged));
				}
			}
			LOG.info("Integrity constraints succesfully parsed.");
		} catch (OWLOntologyChangeException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		} catch (OWLOntologyCreationException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		} catch (UnsupportedICException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	private JFieldVar addField(final String name, final JDefinedClass cls,
			final JType fieldType) {
		String newName = name;

		int i = 0;
		while (cls.fields().containsKey(newName)) {
			newName = name + "" + (++i);
		}

		final JFieldVar fvId = cls.field(JMod.PROTECTED, fieldType, newName);
		final String fieldName = fvId.name().substring(0, 1).toUpperCase()
				+ fvId.name().substring(1);
		final JMethod mSetId = cls.method(JMod.PUBLIC, void.class, "set"
				+ fieldName);
		final JVar v = mSetId.param(fieldType, fvId.name());
		mSetId.body().assign(JExpr._this().ref(fvId), v);
		final JMethod mGetId = cls.method(JMod.PUBLIC, fieldType, "get"
				+ fieldName);
		mGetId.body()._return(fvId);
		return fvId;
	}

	private JDefinedClass ensureCreated(final String pkg, final JCodeModel cm,
			final OWLClass c) {
		if (classes.containsKey(c)) {
			return classes.get(c);
		}

		JDefinedClass cls;

		String name = pkg + validJavaID(c.getIRI().getFragment());

		try {
			cls = cm._class(name);

			cls
					.annotate(
							cz.cvut.kbss.owlpersistence.model.annotations.OWLClass.class)
					.param("iri", c.getIRI().toString());

			final JDocComment dc = cls.javadoc();
			dc.add("This class was generated by the OWL2Java tool.");

			// RDFS label
			final JClass fieldType1 = cm.ref(String.class);
			final JFieldVar fvId1 = addField("label", cls, fieldType1);
			fvId1.annotate(OWLAnnotationProperty.class).param("iri",
					OWLRDFVocabulary.RDFS_LABEL.getIRI().toString());

			// @Types Set<String> types;
			final JClass fieldType2 = cm.ref(Set.class).narrow(String.class);
			final JFieldVar fvId2 = addField("types", cls, fieldType2);
			fvId2.annotate(Types.class);

			// @Id public final String id;
			final JClass fieldType = cm.ref(String.class);
			final JFieldVar fvId = addField("id", cls, fieldType);
			fvId.annotate(Id.class);

			// // public final Map<Object,Set<Object>> other;
			// final JClass cSetN = cm.ref(Set.class).narrow(Object.class);
			// final JClass cMapN =
			// cm.ref(Map.class).narrow(cm.ref(Object.class),
			// cSetN);
			// final JFieldVar fv = cls.field(JMod.PRIVATE, cMapN, "others");
			//
			// // getOther()
			// final JMethod m = cls.method(JMod.PUBLIC, cSetN, "findOther");
			// final JVar p = m.param(Object.class, "property");
			// m.body()._return(fv.invoke("get").arg(p));

		} catch (JClassAlreadyExistsException e) {
			cls = cm._getClass(name);
		}
		classes.put(c, cls);

		return cls;
	}

	private void generateVocabulary(final JCodeModel cm, final JDefinedClass voc) {
		LOG.info("Generating Vocabulary");

		for (final OWLEntity c : merged.getSignature()) {
			String prefix = "";

			if (c.isOWLClass() || c.isOWLDatatype()) {
				prefix = "c_";
			} else if (c.isOWLDataProperty() || c.isOWLObjectProperty()
					|| c.isOWLAnnotationProperty()) {
				prefix = "p_";
			} else if (c.isOWLNamedIndividual()) {
				prefix = "i_";
			}

			// TODO
			if (c.isOWLAnnotationProperty()) {
				continue;
			}

			String id = prefix + validJavaID(c.getIRI().getFragment());

			while (voc.fields().keySet().contains("s_" + id)) {
				id += "_A";
			}

			final String sFieldName = "s_" + id;

			final JFieldVar fv1 = voc.field(JMod.PUBLIC | JMod.STATIC
					| JMod.FINAL, String.class, sFieldName, JExpr.lit(c
					.getIRI().toString()));
			voc.field(JMod.PUBLIC | JMod.STATIC | JMod.FINAL, IRI.class, id, cm
					.ref(IRI.class).staticInvoke("create").arg(fv1));

			entities.put(c, fv1);
		}
	}

	private static String validJavaID(final String s) {
		return s.trim().replace("-", "_");
	}

	private void generateModel(final JCodeModel cm, final String context,
			final String pkg) {
		// model classes
		try {
			for (final OWLPersistenceAnnotation a : ics.get(context)) {
				if (a instanceof ClassParticipationConstraint) {
					final ClassParticipationConstraint cpc = (ClassParticipationConstraint) a;

					final JDefinedClass subj = ensureCreated(pkg, cm, cpc
							.getSubject());
					final JDefinedClass obj = ensureCreated(pkg, cm, cpc
							.getObject());

					final JFieldVar fv;

					final String fieldName = validJavaID(cpc.getPredicate()
							.getIRI().getFragment());

					if (cpc.getMaxPossible() > 1) {
						fv = addField(fieldName, subj, cm.ref(
								java.util.Set.class).narrow(obj));
					} else {
						fv = addField(fieldName, subj, obj);
					}
					fv.annotate(OWLObjectProperty.class).param("iri",
							cpc.getPredicate().getIRI().toString()).param(
							"min", cpc.getMinRequired()).param("max",
							cpc.getMaxPossible()).param("fillerIri",
							cpc.getObject().getIRI().toString());
				} else if (a instanceof NamedIndividualParticipationConstraint) {
					throw new UnsupportedICException("NOT IMPLEMENTED YET");
				} else if (a instanceof DatatypeParticipationConstraint) {
					final DatatypeParticipationConstraint cpc = (DatatypeParticipationConstraint) a;

					final JDefinedClass subj = ensureCreated(pkg, cm, cpc
							.getSubject());

					final JType type = cm._ref(DatatypeTransformer
							.transformOWLType(cpc.getObject()));

					final JFieldVar fv;

					if (cpc.getMaxPossible() > 1) {
						fv = addField(
								cpc.getPredicate().getIRI().getFragment(),
								subj, cm.ref(java.util.Set.class).narrow(type)); // Literal
						// interface
					} else {
						fv = addField(
								cpc.getPredicate().getIRI().getFragment(),
								subj, type); // Literal interface
					}

					fv.annotate(OWLDataProperty.class).param("iri",
							cpc.getPredicate().getIRI().toString()).param(
							"min", cpc.getMinRequired()).param("max",
							cpc.getMaxPossible()).param("fillerIri",
							cpc.getObject().getIRI().toString());
				} else if (a instanceof LiteralParticipationConstraint) {
					throw new UnsupportedICException("NOT IMPLEMENTED YET");
				} else if (a instanceof SubClassOfSpecification) {
					final SubClassOfSpecification scs = (SubClassOfSpecification) a;
					final JDefinedClass subj = ensureCreated(pkg, cm, scs
							.getSubClass());

					final JDefinedClass supClass = ensureCreated(pkg, cm, scs
							.getSuperClass());

					subj._extends(supClass);
				} else {
					throw new UnsupportedICException("Uknown IC type: "
							+ a.getClass().getName());
				}
			}
		} catch (UnsupportedICException e) {
			e.printStackTrace();
		}
	}

	private static Map<URI, URI> getMappings(final String mappingFile) {
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

	public void transform(String context, String p, String dir) {
		LOG.info("Transforming context '" + p + "'.");

		final JCodeModel cm = new JCodeModel();

		try {
			generateVocabulary(cm, cm._class(p + ".Vocabulary"));
			generateModel(cm, context, p + ".model.");

			final File file = new File(dir);
			file.mkdirs();
			cm.build(file);
			LOG.info("Transformation SUCCESFUL.");
		} catch (JClassAlreadyExistsException e1) {
			LOG.log(Level.SEVERE, "Transformation FAILED.", e1);
		} catch (IOException e) {
			LOG.log(Level.SEVERE, "File generation FAILED.", e);
		}
	}
}
