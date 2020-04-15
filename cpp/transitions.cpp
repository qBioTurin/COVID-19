// Keeps indexes of all places with population belonging to any age class.
vector<vector<int>> indexes_age_classes;
// Keeps the current population size for each age class with the time
vector<pair<double,double>> age_class_size;

// Keeps indexes of death places with population belonging to any age class.
vector<vector<int>> indexes_death_age_classes;
// Keeps the current population count of deaths for each age class with the time
vector<pair<double,double>> death_age_class_size;

// Keeps the mapping between a transition and its index in the data structure age_class_size
unordered_map<string,int> transition_idx;

// Contact rates for different combinations of age classes (from file)
static unordered_map <string,double> contact;

// Infection latency rates for different combinations of age classes (from file)
static unordered_map <string,double> latency;

// Strenght of State's initiaitves (from file)
static unordered_map <int,double> alpha;

// Contact rates
static unordered_map <string, double> contact_rates;

// Infection latency rates
static unordered_map <string, double> latency_rates;;

// Number of age classes
static int clss = 5;

// Number of latency classes
static int clss_l = 3;

static double k_param;

// Fraction of contacts while infected respect to while susceptible (indexes: 0 -> s_0, 1 -> s_1 )
static vector<double> eta;

static bool populate_data_structures = true; 

/* Read data from file and fill probabilities */
void read_double(string fname, double& d)
{
  cout << "#### " << fname << "####" << endl;
  ifstream f (fname);
  string line;
  if(f.is_open())
  {
    getline(f,line);
    d = stod(line);
    f.close();
    cout << d << endl;
  }
  else
  {
    std::cerr<<"\nUnable to open " << fname << ": file do not exists\": file do not exists\n";
    exit(EXIT_FAILURE);
  }
}

/* Read data from file and fill probabilities */
void read_vector_double(string fname, vector<double>& v)
{
  cout << "#### " << fname << "####" << endl;
  ifstream f (fname);
  string line;
  if(f.is_open())
  {
    while (getline(f,line))
    {
      v.push_back(stod(line));
      cout << stod(line) << endl; 
    }
    f.close();
  }
  else
  {
    std::cerr<<"\nUnable to open " << fname << ": file do not exists\": file do not exists\n";
    exit(EXIT_FAILURE);
  }
}

/* Read data from file and fill a map<int,double> */
void read_map_int_double(string fname, unordered_map<int,double>& m)
{
  ifstream f (fname);
  string line;
  if(f.is_open())
  {
    int i = 1;
    cout << "#### " << fname << "####" << endl;
    while (getline(f,line))
    {
      m.insert(pair<int,double>(i, stod(line)));
      cout << i << "\t" << m[i] << endl;
      ++i;
    }
    f.close();
  }
  else
  {
    std::cerr<<"\nUnable to open " << fname << ": file do not exists\": file do not exists\n";
    exit(EXIT_FAILURE);
  }
}

/* Read data from file and fill a map<string,double> */
void read_map_string_double_l(string fname, unordered_map<string,double>& m)
{
  ifstream f (fname);
  string line;
  if(f.is_open())
  {
    vector<string> b_cls;
    size_t pos = 0, c_pos = 0, length = 0;
    cout << "#### " << fname << "####" << endl;
    for(int j = 0; j < clss_l; j++)
    {
      b_cls.push_back("sy_s"+to_string(j));
      cout << "         ";
      cout << "sy_s"+to_string(j);
    }
    cout << endl;
    int j = 0;
    while (getline(f,line))
    {
      int i = 0;
      pos = 0;
      c_pos = 0;
      string a_class = "a_a"+to_string(j);
      // read rates
      cout << " a_a"+to_string(j) << " ";
      length = line.length();
      do
      {
        pos = line.find(',',c_pos);
        if( pos == string::npos)
          pos = length;
        m.insert(pair<string,double>(a_class+"_"+b_cls[i], stod(line.substr(c_pos,pos - c_pos))));
        cout << "| " << to_string(m[a_class+"_"+b_cls[i]]) << " ";
        c_pos = pos+1;
        ++i;
      }
      while(pos != length);
      cout << endl;
      ++j;
    }
    f.close();
  }
  else
  {
    std::cerr<<"\nUnable to open " << fname << ": file do not exists\n";
    exit(EXIT_FAILURE);
  }
}

/* Read data from file and fill a map<string,double> */
void read_map_string_double(string fname, unordered_map<string,double>& m)
{
  ifstream f (fname);
  string line;
  if(f.is_open())
  {
    vector<string> b_cls;
    size_t pos = 0, c_pos = 0, length = 0;
    cout << "#### " << fname << "####" << endl;
    for(int j = 0; j < clss; j++)
    {
      b_cls.push_back("b_a"+to_string(j));
      cout << "         ";
      cout << "b_a"+to_string(j);
    }
    cout << endl;
    int j = 0;
    while (getline(f,line))
    {
      int i = 0;
      pos = 0;
      c_pos = 0;
      string a_class = "a_a"+to_string(j);
      // read rates
      cout << " a_a"+to_string(j) << " ";
      length = line.length();
      do
      {
        pos = line.find(',',c_pos);
        if( pos == string::npos)
          pos = length;
        m.insert(pair<string,double>(a_class+"_"+b_cls[i], stod(line.substr(c_pos,pos - c_pos))));
        cout << "| " << to_string(m[a_class+"_"+b_cls[i]]) << " ";
        c_pos = pos+1;
        ++i;
      }
      while(pos != length);
      cout << endl;
      ++j;
    }
    f.close();
  }
  else
  {
    std::cerr<<"\nUnable to open " << fname << ": file do not exists\n";
    exit(EXIT_FAILURE);
  }
}

void init_data_structures()
{
  read_map_string_double("./c_rates", contact);
  read_map_string_double_l("./l_rates", latency);
  read_map_int_double("./alpha", alpha);
  read_double("./k",k_param);
  read_vector_double("./eta",eta);
  /* Initialize data structure */
  for(int i =0; i<clss; i++)
  {
    pair<double,double> p = {-1.0, 0.0};
    age_class_size.push_back(p);
    pair<double,double> p1 = {-1.0, 0.0};
    death_age_class_size.push_back(p1);
    vector<int> v;
    indexes_age_classes.push_back(v);
    vector<int> v1;
    indexes_death_age_classes.push_back(v1);
  }
  populate_data_structures = false;
}

void fill_indexes_age_classes(string a_class, int idx, double *Value, map <string,int>& NumPlaces, const double& time)
{
  regex e (a_class);
  /* Match all that palces used for some measurements */
  regex e1 ("(c_)");
  regex e2 ("(d_)");
  /* Update the timestamp */
  age_class_size[idx].first = time;
  death_age_class_size[idx].first = time;
  /* Search for the indexes and compute the age class size */
  for(auto it=NumPlaces.begin(); it!=NumPlaces.end(); it++)
  {
    smatch m, m1, m2;
    regex_search(it->first,m,e);
    regex_search(it->first,m1,e1);
    regex_search(it->first,m2,e2);
    if(m.size() > 0 && m1.size() == 0 && m2.size() == 0)
    {
      /* Populate the vector of indexes for the current age class */
      indexes_age_classes[idx].push_back(it->second);
      /* Compute the current value for the age class siaze */
      age_class_size[idx].second+=Value[it -> second];
    }
    else if(m.size() > 0 && m1.size() == 0 && m2.size() > 0)
    {
      /* Populate the vector of indexes for the current age class */
      indexes_death_age_classes[idx].push_back(it->second);
      /* Compute the current value for the age class siaze */
      death_age_class_size[idx].second+=Value[it -> second];
    }
  }
}

/* It computes such value one for each timestamp. That is, if the function is called 
 * multiple times at the same time instance the computation is done oly onece, then the
 * value is stored to satisfy the subsequent ones.
 * When the function is called for the very first time, it also fill the data structure
 * coupling each place to a specific age class (see fill_indexes_age_classes).
 */
void compute_age_class_size(int idx, double *Value, map <string,int>& NumPlaces, const double& time)
{
  /* if the value for the age class size is not updated, compute it */
  if( time != age_class_size[idx].first )
  {
    /* Update the timestamp */
    age_class_size[idx].first = time;
    /* Comupute the population size for thi age class at this time */
    age_class_size[idx].second = 0;
    for (auto i = indexes_age_classes[idx].begin(); i != indexes_age_classes[idx].end(); i++)
    {
      age_class_size[idx].second+=Value[*i];
    }
    /* Update the timestamp */
    death_age_class_size[idx].first = time;
    /* Comupute the population size for thi age class at this time */
    death_age_class_size[idx].second = 0;
    for (auto i = indexes_death_age_classes[idx].begin(); i != indexes_death_age_classes[idx].end(); i++)
    {
      death_age_class_size[idx].second+=Value[*i];
    }
  }
}

/* Finds index of the age class corresponding to the infect involved in the contact */
int age_class_size_idx(string transition, double *Value, map <string,int>& NumPlaces, const double& time)
{
  auto it = transition_idx.find(transition);
  int idx = -1;
  if( it != transition_idx.end() )
    idx = it -> second;
  else
  {
    /* regex describing the name of the transitions for each age class */
    regex a0 ("(a_a0){1}(_){0}");   // matches strings containing "a0"
    regex a1 ("(a_a1){1}(_){0}");   // matches strings containing "a1"
    regex a2 ("(a_a2){1}(_){0}");   // matches strings containing "a2"
    regex a3 ("(a_a3){1}(_){0}");   // matches strings containing "a3"
    regex a4 ("(a_a4){1}(_){0}");   // matches strings containing "a4"
    string a_class = "";
    smatch m;
    /* Identify the age class */
    if(regex_search(transition,m,a0))
    {
      a_class = "(a0){1}";
      idx = 0;
    }
    else if(regex_search(transition,m,a1))
    {
      a_class = "(a1){1}";
      idx = 1;
    }
    else if(regex_search(transition,m,a2))
    {
      a_class = "(a2){1}";
      idx = 2;
    }
    else if(regex_search(transition,m,a3))
    {
      a_class = "(a3){1}";
      idx = 3;
    }
    else if(regex_search(transition,m,a4))
    {
      a_class = "(a4){1}";
      idx = 4;
    }
    
    transition_idx[transition] = idx;
    /* Populate the vector with the indexes of this age class */
    if(indexes_age_classes.size() == 0 || indexes_age_classes[idx].size() == 0)
      fill_indexes_age_classes(a_class, idx, Value, NumPlaces, time);
  }
  if( idx == -1 )
    throw std::invalid_argument( "Age class not found! Please, kill me and check the method 'age_class_size'" );
  else
  {	
    compute_age_class_size(idx,Value, NumPlaces, time);
  }
  return idx;
}


// Compute the transition rate according to:
// 	- age classes of the patients involved in the contact
double C(double *Value, map <string,int>& NumTrans, map <string,int>& NumPlaces,const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{
  if( populate_data_structures )
    init_data_structures();
  double rate=0;
  auto c = contact_rates.find(NameTrans[T]);
  if(c != contact_rates.end())
    // The transition has already fired and the rate is in the map 
    rate = c -> second;
  else
  {
    // It's the first time the transition fires, hence the rate has to be computed
    auto it=contact.begin();
    bool found = false;
    while(  it!=contact.end() && !found )
    {
      /* Check the age classes involved in the contact */
      regex e (it->first);
      smatch m;
      if(regex_search(NameTrans[T],m,e))
      {
        /* Check symptoms */
        regex e1("(sy_s1){1}");
        smatch m1;
        regex e2("(sy_s2){1}");
        smatch m2;
        int index = 0; /* s0 */		
        if(regex_search(NameTrans[T],m1,e1))
        {
          index = 1; /* s1 */
        }
        else if(regex_search(NameTrans[T],m2,e2))
        {
          index = 2; /* s2 */
        }
        rate=it->second*(1-eta[index]);  
        found = true;
      }
      ++it;
    }
    if(!found) 
    {
      throw std::invalid_argument( "C: transition " + NameTrans[T] + " not not found! Please, kill me and check the method 'C'" );
    }
    else
      contact_rates.insert(pair<string,double>(NameTrans[T], rate));
  }
  double in;
  modf(time, &in);
  int day = 1 + (int) in; 
  int idx = age_class_size_idx(NameTrans[T], Value, NumPlaces, time);
  double intensity = 1.0/age_class_size[idx].second;
  for (unsigned int k=0; k<Trans[T].InPlaces.size(); k++)
  {
    intensity *= pow(Value[Trans[T].InPlaces[k].Id],Trans[T].InPlaces[k].Card);
  }
  double D = 0.0, N = 0.0;
  for(int c = 0; c < clss; c++)
  {
    D+=death_age_class_size[c].second;
    N+=age_class_size[c].second;
  }
  auto it_day = alpha.find(day);
  while(it_day == alpha.end())
    it_day = alpha.find(--day);
  double gov_action_strength = (1.0-alpha.find(day)->second);
  double population_response = pow((1.0-D/N),k_param);
  return rate*intensity*gov_action_strength*population_response;
}

// Compute the transition rate according to:
// 	- age classes of the patients involved in the contact
double L(double *Value, map <string,int>& NumTrans, map <string,int>& NumPlaces,const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{
  if( populate_data_structures )
    init_data_structures();
  double rate=0;
  auto l = latency_rates.find(NameTrans[T]);
  if(l != latency_rates.end())
    // The transition has already fired and the rate is in the map 
    rate = l -> second;
  else
  {
    // It's the first time the transition fires, hence the rate has to be computed
    auto it=latency.begin();
    bool found = false;
    while(  it!=latency.end() && !found )
    {
      /* Check the age classes involved in the contact */
      regex e (it->first);
      smatch m;
      if(regex_search(NameTrans[T],m,e))
      {
        rate=it->second;  
        cout << NameTrans[T] << " rate = " << rate << endl;
        found = true;
      }
      ++it;
    }
    if(!found) 
    {
      throw std::invalid_argument( "L: transition " + NameTrans[T] + " not not found! Please, kill me and check the method 'C'" );
    }
    else
      latency_rates.insert(pair<string,double>(NameTrans[T], rate));
  }
  double intensity = 1.0;
  for (unsigned int k=0; k<Trans[T].InPlaces.size(); k++)
  {
    intensity *= pow(Value[Trans[T].InPlaces[k].Id],Trans[T].InPlaces[k].Card);
  }
  return rate*intensity;
}
