
inline void mallocedString(std::string const& s, char **ret, int *len)
{
  *len = s.size();
  *ret = static_cast<char*>(malloc(s.size()+1));
  std::memcpy(*ret, s.c_str(), s.size());
  (*ret)[s.size()] = '\0';
}
