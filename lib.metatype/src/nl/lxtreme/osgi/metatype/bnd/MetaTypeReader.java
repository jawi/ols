package nl.lxtreme.osgi.metatype.bnd;


public class MetaTypeReader /* extends WriteResource */
{
  // @formatter:off
//  final Analyzer reporter;
//  Clazz clazz;
//  String interfaces[];
//  Tag metadata = new Tag( "metatype:MetaData", new String[] { "xmlns:metatype",
//      "http://www.osgi.org/xmlns/metatype/v1.1.0" } );
//  Tag ocd = new Tag( this.metadata, "OCD" );
//  Tag designate = new Tag( this.metadata, "Designate" );
//  Tag object = new Tag( this.designate, "Object" );
//
//  // Resource
//  String extra;
//
//  // One time init
//  boolean finished;
//
//  // Designate
//  boolean override;
//  String designatePid;
//  boolean factory;
//
//  // AD
//  Map<MethodDef, Meta.AD> methods = new LinkedHashMap<MethodDef, Meta.AD>();
//
//  // OCD
//  Annotation ocdAnnotation;
//
//  MethodDef method;
//
//  public MetaTypeReader( final Clazz clazz, final Analyzer reporter )
//  {
//    this.clazz = clazz;
//    this.reporter = reporter;
//  }
//
//  /**
//   * @param id
//   * @param name
//   * @param cardinality
//   * @param required
//   * @param deflt
//   * @param type
//   * @param max
//   * @param min
//   * @param optionLabels
//   * @param optionValues
//   */
//
//  static Pattern COLLECTION = Pattern.compile( "(.*(Collection|Set|List|Queue|Stack|Deque))<(L.+;)>" );
//
//  private void addMethod( final MethodDef method, final Meta.AD ad ) throws Exception
//  {
//
//    // Set all the defaults.
//
//    String rtype = method.getGenericReturnType();
//    String id = Configurable.mangleMethodName( method.getName() );
//    String name = Clazz.unCamel( id );
//
//    int cardinality = 0;
//
//    if ( rtype.endsWith( "[]" ) )
//    {
//      cardinality = Integer.MAX_VALUE;
//      rtype = rtype.substring( 0, rtype.length() - 2 );
//    }
//    if ( rtype.indexOf( '<' ) > 0 )
//    {
//      if ( cardinality != 0 )
//      {
//        this.reporter.error(
//            "AD for %s.%s uses an array of collections in return type (%s), Metatype allows either Vector or array",
//            this.clazz.getClassName().getFQN(), method.getName(), method.getType().getFQN() );
//      }
//      Matcher m = COLLECTION.matcher( rtype );
//      if ( m.matches() )
//      {
//        rtype = Clazz.objectDescriptorToFQN( m.group( 3 ) );
//        cardinality = Integer.MIN_VALUE;
//      }
//    }
//
//    Meta.Type type = getType( rtype );
//
//    boolean required = ( ad == null ) || ad.required();
//    String deflt = null;
//    String max = null;
//    String min = null;
//    String[] optionLabels = null;
//    String[] optionValues = null;
//    String description = null;
//
//    TypeRef typeRef = this.reporter.getTypeRefFromFQN( rtype );
//    Clazz c = this.reporter.findClass( typeRef );
//    if ( ( c != null ) && c.isEnum() )
//    {
//      optionValues = parseOptionValues( c );
//    }
//
//    // Now parse the annotation for any overrides
//
//    if ( ad != null )
//    {
//      if ( ad.id() != null )
//      {
//        id = ad.id();
//      }
//      if ( ad.name() != null )
//      {
//        name = ad.name();
//      }
//      if ( ad.cardinality() != 0 )
//      {
//        cardinality = ad.cardinality();
//      }
//      if ( ad.type() != null )
//      {
//        type = ad.type();
//        // if (ad.required() || ad.deflt() == null)
//        // required = true;
//      }
//
//      if ( ad.description() != null )
//      {
//        description = ad.description();
//      }
//
//      if ( ad.optionLabels() != null )
//      {
//        optionLabels = ad.optionLabels();
//      }
//      if ( ad.optionValues() != null )
//      {
//        optionValues = ad.optionValues();
//      }
//
//      if ( ad.min() != null )
//      {
//        min = ad.min();
//      }
//      if ( ad.max() != null )
//      {
//        max = ad.max();
//      }
//
//      if ( ad.deflt() != null )
//      {
//        deflt = ad.deflt();
//      }
//    }
//
//    if ( optionValues != null )
//    {
//      if ( ( optionLabels == null ) || ( optionLabels.length == 0 ) )
//      {
//        optionLabels = new String[optionValues.length];
//        for ( int i = 0; i < optionValues.length; i++ )
//        {
//          optionLabels[i] = Clazz.unCamel( optionValues[i] );
//        }
//      }
//
//      if ( optionLabels.length != optionValues.length )
//      {
//        this.reporter.error( "Option labels and option values not the same length for %s", id );
//        optionLabels = optionValues;
//      }
//    }
//
//    Tag adt = new Tag( this.ocd, "AD" );
//    adt.addAttribute( "name", name );
//    adt.addAttribute( "id", id );
//    adt.addAttribute( "cardinality", cardinality );
//    adt.addAttribute( "required", required );
//    adt.addAttribute( "default", deflt );
//    adt.addAttribute( "type", type );
//    adt.addAttribute( "max", max );
//    adt.addAttribute( "min", min );
//    adt.addAttribute( "description", description );
//
//    if ( ( optionLabels != null ) && ( optionValues != null ) )
//    {
//      for ( int i = 0; i < optionLabels.length; i++ )
//      {
//        Tag option = new Tag( adt, "Option" );
//        option.addAttribute( "label", optionLabels[i] );
//        option.addAttribute( "value", optionValues[i] );
//      }
//    }
//  }
//
//  private String[] parseOptionValues( final Clazz c ) throws Exception
//  {
//    final List<String> values = Create.list();
//
//    c.parseClassFileWithCollector( new ClassDataCollector()
//    {
//      @Override
//      public void field( final Clazz.FieldDef def )
//      {
//        if ( def.isEnum() )
//        {
//          values.add( def.getName() );
//        }
//      }
//    } );
//    return values.toArray( new String[values.size()] );
//  }
//
//  Meta.Type getType( String rtype )
//  {
//    if ( rtype.endsWith( "[]" ) )
//    {
//      rtype = rtype.substring( 0, rtype.length() - 2 );
//      if ( rtype.endsWith( "[]" ) )
//      {
//        throw new IllegalArgumentException( "Can only handle array of depth one" );
//      }
//    }
//
//    if ( "boolean".equals( rtype ) || Boolean.class.getName().equals( rtype ) )
//    {
//      return Meta.Type.Boolean;
//    }
//    else if ( "byte".equals( rtype ) || Byte.class.getName().equals( rtype ) )
//    {
//      return Meta.Type.Byte;
//    }
//    else if ( "char".equals( rtype ) || Character.class.getName().equals( rtype ) )
//    {
//      return Meta.Type.Character;
//    }
//    else if ( "short".equals( rtype ) || Short.class.getName().equals( rtype ) )
//    {
//      return Meta.Type.Short;
//    }
//    else if ( "int".equals( rtype ) || Integer.class.getName().equals( rtype ) )
//    {
//      return Meta.Type.Integer;
//    }
//    else if ( "long".equals( rtype ) || Long.class.getName().equals( rtype ) )
//    {
//      return Meta.Type.Long;
//    }
//    else if ( "float".equals( rtype ) || Float.class.getName().equals( rtype ) )
//    {
//      return Meta.Type.Float;
//    }
//    else if ( "double".equals( rtype ) || Double.class.getName().equals( rtype ) )
//    {
//      return Meta.Type.Double;
//    }
//    else
//    {
//      return Meta.Type.String;
//    }
//  }
//
//  class Find extends ClassDataCollector
//  {
//
//    @Override
//    public void method( final MethodDef mdef )
//    {
//      MetaTypeReader.this.method = mdef;
//      MetaTypeReader.this.methods.put( mdef, null );
//    }
//
//    @Override
//    public void annotation( final Annotation annotation )
//    {
//      try
//      {
//        Meta.OCD ocd = annotation.getAnnotation( Meta.OCD.class );
//        Meta.AD ad = annotation.getAnnotation( Meta.AD.class );
//        if ( ocd != null )
//        {
//          MetaTypeReader.this.ocdAnnotation = annotation;
//        }
//        if ( ad != null )
//        {
//          assert MetaTypeReader.this.method != null;
//          MetaTypeReader.this.methods.put( MetaTypeReader.this.method, ad );
//        }
//      }
//      catch ( Exception e )
//      {
//        MetaTypeReader.this.reporter.error( "Error during annotation parsing %s : %s", MetaTypeReader.this.clazz, e );
//        e.printStackTrace();
//      }
//    }
//
//  }
//
//  @Override
//  public void write( final OutputStream out ) throws IOException
//  {
//    try
//    {
//      finish();
//    }
//    catch ( Exception e )
//    {
//      throw new RuntimeException( e );
//    }
//    PrintWriter pw = new PrintWriter( new OutputStreamWriter( out, "UTF-8" ) );
//    pw.println( "<?xml version='1.0'?>" );
//    this.metadata.print( 0, pw );
//    pw.flush();
//  }
//
//  void finish() throws Exception
//  {
//    if ( !this.finished )
//    {
//      this.finished = true;
//      this.clazz.parseClassFileWithCollector( new Find() );
//      Meta.OCD ocd = null;
//      if ( this.ocdAnnotation != null )
//      {
//        ocd = this.ocdAnnotation.getAnnotation( Meta.OCD.class );
//      }
//      else
//      {
//        ocd = Configurable.createConfigurable( Meta.OCD.class, new HashMap<String, Object>() );
//      }
//
//      // defaults
//      String id = this.clazz.getClassName().getFQN();
//      String name = Clazz.unCamel( this.clazz.getClassName().getShortName() );
//      String description = null;
//      String localization = id;
//      boolean factory = this.factory;
//
//      if ( ocd.id() != null )
//      {
//        id = ocd.id();
//      }
//
//      if ( ocd.name() != null )
//      {
//        name = ocd.name();
//      }
//
//      if ( ocd.localization() != null )
//      {
//        localization = ocd.localization();
//      }
//
//      if ( ocd.description() != null )
//      {
//        description = ocd.description();
//      }
//
//      String pid = id;
//      if ( this.override )
//      {
//        pid = this.designatePid;
//        factory = this.factory;
//        id = this.designatePid; // for the felix problems
//      }
//      else
//      {
//        if ( this.ocdAnnotation.get( "factory" ) != null )
//        {
//          factory = true;
//        }
//      }
//
//      this.ocd.addAttribute( "name", name );
//      this.ocd.addAttribute( "id", id );
//      this.ocd.addAttribute( "description", description );
//      this.ocd.addAttribute( "localization", localization );
//
//      // do ADs
//      for ( Map.Entry<MethodDef, Meta.AD> entry : this.methods.entrySet() )
//      {
//        addMethod( entry.getKey(), entry.getValue() );
//      }
//
//      this.designate.addAttribute( "pid", pid );
//      if ( factory )
//      {
//        this.designate.addAttribute( "factoryPid", pid );
//      }
//
//      this.object.addAttribute( "ocdref", id );
//
//    }
//  }
//
//  public void setDesignate( final String pid, final boolean factory )
//  {
//    this.override = true;
//    this.factory = factory;
//    this.designatePid = pid;
//  }
//
//  @Override
//  public long lastModified()
//  {
//    return 0;
//  }
  // @formatter:on
}
