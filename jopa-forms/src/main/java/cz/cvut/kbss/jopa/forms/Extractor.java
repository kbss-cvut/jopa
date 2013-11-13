
package cz.cvut.kbss.jopa.forms;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.graph.GraphAdapterBuilder;
import com.google.gson.typeadapters.RuntimeTypeAdapterFactory;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import org.json.simple.JSONObject;


public class Extractor {

    private Class[] classes;
   
    public Extractor(String namespace) throws ClassNotFoundException, IOException {
        classes = getClasses(namespace);
    }
    
    public Extractor(Class[] classes) {
        this.classes = classes;
    }
    
    public Class[] getClasses() {
        return classes;
    }

    public void setClasses(Class[] classes) {
        this.classes = classes;
    }
    
    public JSONObject getSchema() throws ClassNotFoundException {
        JSONObject definitions = new JSONObject();
         for (int i = 0; i < classes.length; i++) {
            JSONObject fields = getFields(classes[i]);
            definitions.put(classes[i].getName(), fields);
        }

//        JSONObject schema = new JSONObject();
//        schema.put("type", "object");
//        schema.put("$schema", "http://json-schema.org/draft-04/schema#");
//        schema.put("definitions", definitions);
        
        return definitions;
    }
    
    public JSONObject getFields(Class inputClass) throws ClassNotFoundException {        
        Field[] fs = inputClass.getDeclaredFields();
        
        JSONObject obj = new JSONObject();
        obj.put("id", inputClass.getName());
        obj.put("type", "object");
        
        OWLClass owlclass = (OWLClass) inputClass.getAnnotation(OWLClass.class);
        if (owlclass != null) {
            obj.put("iri", owlclass.iri());
        }
        
//        Class sup = inputClass.getSuperclass();
//        String namespace = inputClass.getPackage().getName();
//        if (sup != null && !inputClass.getName().equals(namespace+".Thing")) {
//            String supName = sup.getName();
//            if (supName.equals("java.lang.Object")) {
//                supName = namespace+".Thing";
//            }
//            JSONObject ref = new JSONObject();
//            ref.put("$ref", "#/definitions/"+supName);
//            obj.put("extends", ref);
//        }
        
        // misto extends spis pouziti types
        
        JSONObject properties = new JSONObject();
        obj.put("properties", properties);

        // all fields from Structure
        for (Field field : fs) {
            if (field.getName().equals("properties") || field.getName().equals("types")) {
                // skip meta properties
                continue;
            }
            JSONObject prop = new JSONObject();
            properties.put(field.getName(), prop);
            
//            if (hasProperAnnotations(field)) {

             setType(prop, field.getGenericType().toString());
            
                ParticipationConstraints pc = field.getAnnotation(ParticipationConstraints.class);
                if (pc != null) {
                    ParticipationConstraint pcs[] = pc.value();
                    for (ParticipationConstraint innerPc : pcs) {
                        if (prop.get("type").equals("array")) {
                          if (innerPc.max() >= 0) {
                              prop.put("maxItems", innerPc.max());
                          }
                          if (innerPc.min() >= 0) {
                              prop.put("minItems", innerPc.min());
                          }
                        } else {
                            if (innerPc.min() == 1) {
                                prop.put("required", true);
                            }
                        }
                    }
                }
                
                //@OWLAnnotationProperty(iri = CommonVocabulary.RDFS_LABEL)
                // @Id(generated = true)
                // @Properties - asi se na to vysrat a dat additional properties true
                // @Types
                
                //@Types
                //protected Set<String> types;
                
                //     @Properties
                // protected Map<String, Set<String>> properties;  
                // protected Set<Factor> hasFactor;

                // TODO: maybe determine by annotations?
                
                
                OWLDataProperty owldataprop = field.getAnnotation(OWLDataProperty.class);
                if (owldataprop != null) {
                    prop.put("iri", owldataprop.iri());
                }
                
                OWLObjectProperty owlobjprop = field.getAnnotation(OWLObjectProperty.class);
                if (owlobjprop != null) {
                    prop.put("iri", owlobjprop.iri());
                }
                
                String iri = getOwlObjectIri(field);               
                if (iri != null) {
                    prop.put("iri-ic", iri);
                }
                
//                    if (isDataProp(field) || field.getAnnotation(OWLAnnotationProperty.class) != null) {
//                        //dpff.setDeclaringClass(inputClass);
//                        
//                        
//                    } else if (isObjectProp(field)) {                        
//                        
//                        
//                        prop.put("type", field.getGenericType());
                        //opff.setDeclaringClass(inputClass);
                        
                        //iri daneho fieldu
                        
                        //moznosti pro dany field, ktere uz mame v ontologiich
                        //List<SelectItem> possibilities = getSelectFromList(getObjPropPossibilities(iri));
                        //subfieldy daneho fieldu
//                        Field[] subFs = getSubFields(field);
//                        if (subFs != null && subFs.length > 0) {
//
//                            //inputboxy a selectboxy pro vztvareni nove objectProperty
//                            List<FormField> subInputBoxes = getSubInputBoxes(subFs);
//                            //nejsou potreba moznosti na vyber => nabidnu input boxy pro vlozeni nove object property
//                            opff.setPossibilities(possibilities);
//                            //muzu vybrat moznost nebo vlozit novou
//                            opff.setSubFields(subInputBoxes);
//                        }
//                    }
                
//            }
        }

        
            
        return obj;
        
    }

    protected void setType(JSONObject obj, String type) {
        String nu = null;
        if (type.equals("class java.lang.String")) {
            nu =  "string";
        } else if (type.equals("class java.lang.Boolean")) {
            nu =  "boolean";
        } else if (type.equals("class java.lang.Integer")) {
            nu =  "integer";
        } else if (type.equals("class java.lang.Double")) {
            nu =  "number";
        } else if (type.equals("class java.util.Date")) {
            nu =  "datetime";
        }
        
        if (nu != null) {
            obj.put("type", nu);
            return;
        }
        
        if (type.startsWith("java.util.Set<")) {
            obj.put("type", "array");
            JSONObject items = new JSONObject();
            setType(items, type.substring(14, type.length()-1));
            //items.put("type", type.substring(14, type.length()-1));
            obj.put("items", items);
            return;
        }
        
        obj.put("type", type.replace("class ", ""));
    }
    
    protected boolean isObjectProp(Field f) {
        return f.getAnnotation(OWLObjectProperty.class) != null;
    }

    protected boolean isDataProp(Field f) {
        return f.getAnnotation(OWLDataProperty.class) != null;
    }
    
    protected String getOwlObjectIri(Field f) {
        ParticipationConstraints pc = f.getAnnotation(ParticipationConstraints.class);
        if (pc != null) {
            ParticipationConstraint pcs[] = pc.value();
            return pcs[0].owlObjectIRI();
        } else {
            return null;
        }
    }

    /**
     * Scans all classes accessible from the context class loader which belong to the given package and subpackages.
     *
     * @param packageName The base package
     * @return The classes
     * @throws ClassNotFoundException
     * @throws IOException
     */
    public static Class[] getClasses(String packageName) throws ClassNotFoundException, IOException {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        assert classLoader != null;
        String path = packageName.replace('.', '/');
        Enumeration<URL> resources = classLoader.getResources(path);
        List<File> dirs = new ArrayList<File>();
        while (resources.hasMoreElements()) {
            URL resource = resources.nextElement();
            dirs.add(new File(resource.getFile()));
        }
        ArrayList<Class> classes = new ArrayList<Class>();
        for (File directory : dirs) {
            classes.addAll(findClasses(directory, packageName));
        }
        return classes.toArray(new Class[classes.size()]);
    }

    /**
     * Recursive method used to find all classes in a given directory and subdirs.
     *
     * @param directory   The base directory
     * @param packageName The package name for classes found inside the base directory
     * @return The classes
     * @throws ClassNotFoundException
     */
    protected static List<Class> findClasses(File directory, String packageName) throws ClassNotFoundException {
        List<Class> classes = new ArrayList<Class>();
        if (!directory.exists()) {
            return classes;
        }
        File[] files = directory.listFiles();
        for (File file : files) {
            if (file.isDirectory()) {
                assert !file.getName().contains(".");
                classes.addAll(findClasses(file, packageName + "." + file.getName()));
            } else if (file.getName().endsWith(".class")) {
                classes.add(Class.forName(packageName + '.' + file.getName().substring(0, file.getName().length() - 6)));
            }
        }
        return classes;
    }
    
    // Remove this method, only for testing
    public Gson createGsonBuilder() {
        GsonBuilder gsonBuilder = new GsonBuilder()
            .setPrettyPrinting();
        
        GraphAdapterBuilder adapter = new GraphAdapterBuilder();
                
        for (int i = 0; i < classes.length; i++) {
            adapter.addType(classes[i]);
            gsonBuilder.registerTypeAdapterFactory(RuntimeTypeAdapterFactory.of(classes[i], "$class").registerSubtype(classes[i], classes[i].getCanonicalName()));
        }       
        adapter
                                              .registerOn(gsonBuilder);
        Gson gson = gsonBuilder.create();
        
        return gson;
    }
}
