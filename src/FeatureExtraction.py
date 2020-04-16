import itertools
import nltk
import numpy
import treetaggerwrapper
import nltk.classify
import csv
import unicodedata


class classify:

    """ Reads the csv file and writes each row in a postition of a list"""
    def _readCsv(self,file):
        cases = []
        #with open(file, 'r', encoding="utf8") as f:
        with open(file, 'r') as f:
            reader = csv.reader(f)
            for row in reader:
                cases.append(row)
        return cases

    def _readPlaces(self,file):
        with open(file, 'r', encoding="utf8") as f:
            lugares = f.readlines()
        line = ' '.join(lugares)
        line= line.lower()
        lugares = line.split()
        lugares=list(set(lugares))
        return lugares


    """Stores every word and bigram in the specified file """
    def _getWords(self,list_cases):
        words= []
        lugares = self._readPlaces("../scopus/tabooWords.es")
        conteoWords=[]
        colNames = ["Words", "Stops","CC","CD","DT","EX","FW","IN","JJ","JJR","JJS","LS","MD","NN","NNS","NP","NPS","PDT","POS","PP","PP$","RB","RBR","RBS","RP","SYM","TO","UH","VB","VBD","VBG","VBN","VBP","VBZ","WDT","WP","WP$","WRB"]

        #colNames= ["Words","Stops","ACRNM","ADJ","ADV","ALFP","ALFS","ART","BACKSLASH","CARD","CC","CCAD","CCNEG","CM","CODE","COLON","CQUE","CSUBF","CSUBI","CSUBX","DASH","DM","DOTS","FO","FS","INT","ITJN","LP","NC","NEG","NMEA","NMON","NP","ORD","PAL","PDEL","PE","PERCT","PNC","PPC","PPO","PPX","PREP","PREP","PREP/DEL","QT","QU","REL","RP","SE","SEMICOLON","SLASH","SYM","UMMX","VCLIger","VCLIinf","VCLIfin","VEadj","VEfin","VEger","VEinf","VHadj","VHfin","VHger","VHinf","VLadj","VLfin","VLger","VLinf","VMadj","VMfin","VMger","VMinf","VSadj","VSfin","VSger","VSinf"]
        conteoWords.append(colNames)

        #Each line is tokenized and its words and bigrams are stored in a list
        #for line in sentences_file:
        for line in list_cases:
            # Storing all line words
            sentence_words, conteo = self._doNLP(line[0], 'english',lugares)

            sentence_words = list(set(sentence_words))
            words.append(sentence_words)
            conteoWords.append(conteo)

        return wo:q
rds,conteoWords

    """ Does stopwords lematization and tokenize"""
    def _doNLP(self,sentence,language,lugares):
        # every line word is obtained
        line = sentence.lower()
        #line = nltk.re.sub("\d+", " ", line)
        #line = nltk.re.sub("x(x)+", " ", line)
        #line = nltk.re.sub("L.E.", " ", line)
        #line = nltk.re.sub("L.E.Cr.", " ", line)
        #line = nltk.re.sub("L.E. Cr.", " ", line)
        #line = nltk.re.sub("Art.", " ", line)

        conteos= []
        #categorias=["ACRNM","ADJ","ADV","ALFP","ALFS","ART","BACKSLASH","CARD","CC","CCAD","CCNEG","CM","CODE","COLON","CQUE","CSUBF","CSUBI","CSUBX","DASH","DM","DOTS","FO","FS","INT","ITJN","LP","NC","NEG","NMEA","NMON","NP","ORD","PAL","PDEL","PE","PERCT","PNC","PPC","PPO","PPX","PREP","PREP","PREP/DEL","QT","QU","REL","RP","SE","SEMICOLON","SLASH","SYM","UMMX","VCLIger","VCLIinf","VCLIfin","VEadj","VEfin","VEger","VEinf","VHadj","VHfin","VHger","VHinf","VLadj","VLfin","VLger","VLinf","VMadj","VMfin","VMger","VMinf","VSadj","VSfin","VSger","VSinf"]

        categorias=["CC","CD","DT","EX","FW","IN","JJ","JJR","JJS","LS","MD","NN","NNS","NP","NPS","PDT","POS","PP","PP$","RB","RBR","RBS","RP","SYM","TO","UH","VB","VBD","VBG","VBN","VBP","VBZ","WDT","WP","WP$","WRB"]

        conteos.append(len(line.split()))
        conteos.append(line.count('.'))
        line = nltk.re.sub("[\*\+\º\ª\!\·\$\%\&\/\(\)\=\?\¿\^\`\[\]\¨\´\{\}\-\_\,\;\.\:\>\<\¿\¡\@\'\"]", " ", line)
        tagger = treetaggerwrapper.TreeTagger(TAGLANG='en')
        tags = tagger.tag_text(line)
        tags2 = treetaggerwrapper.make_tags(tags)
        flat_tag = [item for sublist in tags2 for item in sublist]
        for cat in categorias:
            conteos.append(flat_tag.count(cat))
        words = []
        for token in tags2:
            if (token.lemma):
                a = token.lemma
                words.append(a)
        final = []
        for word in words:
            if '|' not in word:
                final.append(word)
            else:
                final.append(word.split('|', 1)[0])

        # This is the simple way to remove stop words
        important_words = []
        cachedStopWords = set(nltk.corpus.stopwords.words(language))
        #cachedStopWords.update(lugares)
        for word in final:
            if (word not in cachedStopWords) and (len(word)>2):
                important_words.append(word)

        return important_words, conteos

    """ Transform a sentence into features list to train /classify"""
    def _getFeatures(self,sentence,best_features=None,sentiment='pos'):
        lugares = self._readPlaces("../scopus/tabooWords.es")
        if(type(sentence) is str):
            sentence_words, conteos=self._doNLP(sentence,'spanish',lugares)
        elif(type(sentence) is list):
            sentence_words, conteos = self._doNLP(sentence[0], 'spanish',lugares)

        features_list =[]
        for word in best_features:
            aux=sentence_words.count(word)
            features_list.append(sentence_words.count(word))
        features_list.append(int(sentiment=='pos'))

        return features_list

    """ Prepare the examples of texts for training"""
    def _trainPrep(self,pos_cases_train,neg_cases_train,num_pos,num_total):
        # obtain every word and bigram in both files
        pos_words,pos_conteo = self._getWords(pos_cases_train)
        neg_words,neg_conteo = self._getWords(neg_cases_train)

        with open('conteoPosHotel.csv', 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerows(pos_conteo)
        with open('conteoNegHotel.csv', 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerows(neg_conteo)

        # make all list iterable
        pos_words = list(itertools.chain(*pos_words))
        neg_words = list(itertools.chain(*neg_words))
        pos_conteo = list(itertools.chain(*pos_conteo))
        neg_conteo = list(itertools.chain(*neg_conteo))

        best_feat=pos_words+neg_words
        best_feat = list(set(best_feat))
        pos_features_train = [best_feat+["Truthfulness"]]

        neg_features_train = []

        # obtain best words taking into account the gain of information

        # Each line is tokenized and ist words and bigrams are used to create positive features
        for line in pos_cases_train:
            pos_features_train.append(self._getFeatures(line, best_feat,'pos'))

        # Each line is tokenized and ist words and bigrams are used to create negative features
        for line in neg_cases_train:
            neg_features_train.append(self._getFeatures(line, best_feat,'neg'))




        return pos_features_train,neg_features_train

    """ Train de clasiffier using the specified files """
    def train(self, positive_file, negative_file):

        #obtain a list of positive cases
        pos_cases=self._readCsv(positive_file)
        #obtain a list of negative cases
        neg_cases = self._readCsv(negative_file)
        num_pos=len(pos_cases)
        num_neg=len(neg_cases)
        num_total=num_pos+num_neg


        pos_features_train,neg_features_train= self._trainPrep(pos_cases,neg_cases,num_pos,num_total)
        #prepare for training. Obtaining the feature testing set
        train_features = pos_features_train + neg_features_train

        print("Training instances:", len(train_features))
        with open('HotelPaperWords.csv', 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerows(train_features)



classifierLara = classify()
#classifierLara.train("../corpus/denunciasVerdaderasNoviembre2016.csv","../corpus/denunciasFalsasNoviembre2016.csv")
#classifierLara.train("../corpus/denunciasVerdaderasMayo2017.csv","../corpus/denunciasFalsasMayo2017.csv")
classifierLara.train("../corpus/negativeTruthfulOtt14.csv","../corpus/negativeDeceptiveOtt14.csv")
#classifierLara.train("../corpus/hotel_true.csv","../corpus/hotel_false.csv")
#classifierLara.train("../corpus/truthfulOtt2011.csv","../corpus/deceptiveOtt2011.csv")
